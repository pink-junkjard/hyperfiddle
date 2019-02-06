(ns hyperfiddle.ui.popover
  (:require
    [cats.core :refer [fmap]]
    [cats.monad.either :refer [left right branch]]
    [contrib.css :refer [css]]
    [contrib.ct :refer [unwrap]]
    [contrib.data :refer [dissoc-nils]]
    [contrib.eval :as eval]
    [contrib.keypress :refer [with-keychord]]
    [contrib.reactive :as r]
    [contrib.pprint :refer [pprint-str]]
    [contrib.string :refer [blank->nil]]
    [contrib.try$ :refer [try-either either->promise]]
    [contrib.ui.tooltip :refer [tooltip tooltip-props]]
    [hypercrud.browser.base :as base]
    [hypercrud.browser.context :as context]
    [hypercrud.util.branch :as branch]
    [hyperfiddle.actions :as actions]
    [hyperfiddle.api]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.ui.iframe :refer [iframe-cmp]]
    [promesa.core :as p]
    [re-com.core :as re-com]
    [taoensso.timbre :as timbre]))


(let [safe-eval-string #(try-either (eval/eval-expr-str! %))
      memoized-read-string (memoize safe-eval-string)]
  (defn memoized-read-txfn+ [kw-str]                              ; TODO migrate type to keyword
    (if (blank->nil kw-str)
      (memoized-read-string kw-str)
      (right (constantly nil)))))

(defn- with-swap-fn [link-ref ctx f]
  (let [{:keys [:link/tx-fn] :as link} @link-ref]
    (-> (either->promise
          (memoized-read-txfn+ tx-fn))
        (p/then
          (fn [user-txfn]
            (p/promise
              (fn [resolve! reject!]
                (let [swap-fn-async (fn []
                                      (let [result (let [[e a v] @(:hypercrud.browser/eav ctx)
                                                         result (hyperfiddle.api/txfn user-txfn e a v ctx)]
                                                     ; txfn may be sync or async
                                                     (if-not (p/promise? result) (p/resolved result) result))]
                                        ; let the caller of this :stage fn know the result
                                        ; This is super funky, a swap-fn should not be effecting, but seems like it would work.
                                        (-> result (p/branch (fn [v] (resolve! nil))
                                                             (fn [e]
                                                               (reject! e)
                                                               (timbre/warn e))))

                                        ; return the result to the action, it could be a promise
                                        result))]
                  (runtime/dispatch! (:peer ctx) [:txfn (:link/class link) (:link/path link)])
                  (f swap-fn-async))))))
        ; todo something better with these exceptions (could be user error)
        (p/catch (fn [err] (js/alert (pprint-str err)))))))

(defn stage! [link-ref popover-id child-branch ctx r-popover-data props]
  (let [f (fn [swap-fn-async]
            ; the swap-fn could be determined via the link rel
            (->> (actions/stage-popover (:peer ctx) child-branch
                                        #(->> (swap-fn-async)
                                              (fmap
                                                (fn [tx]
                                                  (-> {:tx {(or (hypercrud.browser.context/uri ctx)
                                                                ; https://github.com/hyperfiddle/hyperfiddle/issues/816
                                                                (hypercrud.browser.context/uri "$" ctx))
                                                            tx}
                                                       :app-route (if-let [f (::redirect props)] (f @r-popover-data))}
                                                      dissoc-nils))))
                                        (actions/close-popover (:branch ctx) popover-id))
                 (runtime/dispatch! (:peer ctx))))]
    (with-swap-fn link-ref ctx f)))

(defn close! [popover-id ctx]
  (runtime/dispatch! (:peer ctx) (actions/close-popover (:branch ctx) popover-id)))

(defn cancel! [popover-id child-branch ctx]
  (runtime/dispatch! (:peer ctx) (actions/batch
                                   (actions/close-popover (:branch ctx) popover-id)
                                   (actions/discard-partition child-branch))))

(defn managed-popover-body [route popover-id ?child-branch link-ref ctx props]
  (let [popover-ctx-pre (cond-> (context/clean ctx)         ; hack clean for block level errors
                                ?child-branch (assoc :branch ?child-branch))
        +popover-ctx-post (base/data-from-route route popover-ctx-pre)
        r-popover-data (r/>>= :hypercrud.browser/result +popover-ctx-post) ; focus the fiddle at least then call @(context/data) ?
        popover-invalid (->> +popover-ctx-post (unwrap (constantly nil)) context/tree-invalid?)]
    [:div.hyperfiddle-popover-body                          ; wrpaper helps with popover max-width, hard to layout without this
     ; NOTE: this ctx logic and structure is the same as the popover branch of browser-request/recurse-request
     [iframe-cmp popover-ctx-pre {:route route}]            ; cycle
     (when ?child-branch
       [:button {:on-click (r/partial stage! link-ref popover-id ?child-branch ctx r-popover-data props)
                 :disabled popover-invalid} "stage"])
     (if ?child-branch
       [:button {:on-click #(cancel! popover-id ?child-branch ctx)} "cancel"]
       [:button {:on-click #(close! popover-id ctx)} "close"])
     #_[:pre (pr-str route)]]))

(defn open! [route popover-id child-branch ctx]
  (runtime/dispatch! (:peer ctx)
                     (if child-branch
                       (actions/add-partition (:peer ctx) route child-branch (::runtime/branch-aux ctx)
                                              (actions/open-popover (:branch ctx) popover-id))
                       (actions/open-popover (:branch ctx) popover-id))))

(defn- show-popover? [popover-id ctx]
  (runtime/state (:peer ctx) [::runtime/partitions (:branch ctx) :popovers popover-id]))

(defn- wrap-with-tooltip [popover-id ctx props child]
  ; omit the formula tooltip when popover is open
  (if @(show-popover? popover-id ctx)
    child
    [tooltip (tooltip-props (:tooltip props)) child]))

(defn run-txfn! [link-ref ctx props]
  (let [popover-data nil]
    (letfn [(f [swap-fn-async]
              (-> (->> (swap-fn-async)
                       (fmap (fn [tx]
                               (-> {:tx {(hypercrud.browser.context/uri ctx) tx}
                                    :app-route (if-let [f (::redirect props)] (f popover-data))}
                                   dissoc-nils))))
                  (p/then
                    (fn [{:keys [tx app-route]}]
                      (->> (actions/with-groups (:peer ctx) (:branch ctx) tx :route app-route)
                           (runtime/dispatch! (:peer ctx)))))))]
      (with-swap-fn link-ref ctx f))))

(defn effect-cmp [link-ref ctx props label]
  (let [props (-> props
                  (assoc :on-click (r/partial run-txfn! link-ref ctx props))
                  ; use twbs btn coloring but not "btn" itself
                  (update :class css (let [txfn (->> @(r/fmap :link/tx-fn link-ref) memoized-read-txfn+ (unwrap (constantly nil)))]
                                       (if-not (contains? (methods hyperfiddle.api/txfn) txfn)
                                         "btn-outline-danger"
                                         "btn-warning"))))]
    [:button (select-keys props [:class :style :disabled :on-click])
     [:span (str label "!")]]))

(defn popover-cmp [link-ref ctx visual-ctx props label]
  ; try to auto-generate branch/popover-id from the product of:
  ; - link's :db/id
  ; - route
  ; - visual-ctx's data & path (where this popover is being drawn NOT its dependencies)
  (let [child-branch (let [child-id-str (-> [(hypercrud.browser.context/tempid visual-ctx)
                                             @(r/fmap :db/id link-ref)
                                             (:route props)
                                             @(r/fmap (r/partial hypercrud.browser.context/reagent-entity-key ctx) (:hypercrud.browser/fiddle ctx))]
                                            hash str)]
                       (branch/encode-branch-child (:branch ctx) child-id-str))
        popover-id child-branch                             ; just use child-branch as popover-id
        child-branch (when @(r/fmap (r/comp some? blank->nil :link/tx-fn) link-ref)
                       child-branch)
        btn-props (-> props
                      ;(dissoc :route :tooltip ::redirect)
                      (assoc :on-click (r/partial open! (:route props) popover-id child-branch ctx))
                      ; use twbs btn coloring but not "btn" itself
                      (update :class css "btn-default"))]
    [wrap-with-tooltip popover-id ctx (select-keys props [:class :on-click :style :disabled :tooltip])
     [with-keychord
      "esc" #(do (js/console.warn "esc") (if child-branch
                                           (cancel! popover-id child-branch ctx)
                                           (close! popover-id ctx)))
      [re-com/popover-anchor-wrapper
       :showing? (show-popover? popover-id ctx)
       :position :below-center
       :anchor [:button (select-keys btn-props [:class :style :disabled :on-click])
                [:span (str label "▾")]]
       :popover [re-com/popover-content-wrapper
                 :no-clip? true
                 :body [managed-popover-body (:route props) popover-id child-branch link-ref ctx props]]]]]))
