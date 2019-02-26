(ns hyperfiddle.ui.popover
  (:require
    [cats.core :refer [fmap]]
    [contrib.css :refer [css]]
    [contrib.ct :refer [unwrap]]
    [contrib.data :refer [dissoc-nils]]
    [contrib.eval :as eval]
    [contrib.keypress :refer [with-keychord]]
    [contrib.reactive :as r]
    [contrib.pprint :refer [pprint-str]]
    [contrib.string :refer [blank->nil]]
    [contrib.try$ :refer [try-promise]]
    [contrib.ui.tooltip :refer [tooltip tooltip-props]]
    [hypercrud.browser.base :as base]
    [hypercrud.browser.context :as context]
    [hyperfiddle.actions :as actions]
    [hyperfiddle.api]
    [hyperfiddle.branch :as branch]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.ui.iframe :refer [iframe-cmp]]
    [promesa.core :as p]
    [re-com.core :as re-com]
    [taoensso.timbre :as timbre]))


(let [safe-eval-string #(try-promise (eval/eval-expr-str! %))
      memoized-eval-string (memoize safe-eval-string)]
  (defn- with-swap-fn [link-ref ctx f]
    (let [{:keys [:link/tx-fn] :as link} @link-ref]
      (-> (if (blank->nil tx-fn)
            (memoized-eval-string tx-fn)                    ; TODO migrate type to keyword
            (p/resolved (constantly nil)))
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
          (p/catch (fn [err] (js/alert (pprint-str err))))))))

(defn stage! [link-ref popover-id child-branch ctx r-popover-data props]
  (let [f (fn [swap-fn-async]
            ; the swap-fn could be determined via the link rel
            (->> (actions/stage-popover (:peer ctx) child-branch
                                        #(->> (swap-fn-async)
                                              (fmap
                                                (fn [tx]
                                                  (-> {:tx {(or (hypercrud.browser.context/dbname ctx) "$") ; https://github.com/hyperfiddle/hyperfiddle/issues/816
                                                            tx}
                                                       :app-route (if-let [f (::redirect props)] (f @r-popover-data))}
                                                      dissoc-nils))))
                                        (actions/close-popover (:branch ctx) popover-id))
                 (runtime/dispatch! (:peer ctx))))]
    (with-swap-fn link-ref ctx f)))

(defn close! [popover-id ctx]
  (runtime/dispatch! (:peer ctx) (actions/close-popover (:branch ctx) popover-id)))

(defn cancel! [popover-id child-branch ctx]
  (runtime/dispatch! (:peer ctx) (fn [dispatch! get-state]
                                   (dispatch!
                                     (apply actions/batch
                                            (actions/close-popover (:branch ctx) popover-id)
                                            (actions/discard-partition get-state child-branch))))))

(defn- branched-popover-body [route popover-id child-branch-id link-ref ctx props]
  (let [popover-ctx-pre (-> (context/clean ctx)             ; hack clean for block level errors
                            (assoc :branch child-branch-id
                                   :hyperfiddle.ui/error-with-stage? true))
        +popover-ctx-post (base/data-from-route route popover-ctx-pre)
        r-popover-data (r/>>= :hypercrud.browser/data +popover-ctx-post)
        popover-invalid (->> +popover-ctx-post (unwrap (constantly nil)) context/tree-invalid?)]
    [:<>
     [iframe-cmp popover-ctx-pre {:route route}]            ; cycle
     [:button {:on-click (r/partial stage! link-ref popover-id child-branch-id ctx r-popover-data props)
               :disabled popover-invalid} "stage"]
     [:button {:on-click #(cancel! popover-id child-branch-id ctx)} "cancel"]]))

(defn- non-branched-popover-body [route popover-id ctx]
  [:<>
   [iframe-cmp (context/clean ctx) {:route route}]          ; cycle
   [:button {:on-click #(close! popover-id ctx)} "close"]])

(defn- open-branched-popover! [rt parent-branch-id child-branch-id popover-id route]
  (fn [dispatch! get-state]
    (dispatch! (actions/batch [:create-partition child-branch-id]
                              [:partition-route child-branch-id route]))
    (-> (actions/bootstrap-data rt child-branch-id actions/LEVEL-GLOBAL-BASIS)
        (p/finally (fn [] (dispatch! (actions/open-popover parent-branch-id popover-id)))))))

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
                               (-> {:tx {(hypercrud.browser.context/dbname ctx) tx}
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
                  (update :class css "btn-warning"))]
    [:button props [:span (str label "!")]]))

(defn popover-cmp [link-ref ctx visual-ctx props label]
  ; try to auto-generate branch/popover-id from the product of:
  ; - link's :db/id
  ; - route
  ; - visual-ctx's data & path (where this popover is being drawn NOT its dependencies)
  (let [child-branch-id (let [relative-id (-> [(hypercrud.browser.context/tempid visual-ctx)
                                               @(r/fmap :db/id link-ref)
                                               (:route props)
                                               @(r/fmap (r/partial hypercrud.browser.context/stable-entity-key ctx) (:hypercrud.browser/fiddle ctx))]
                                              hash str)]
                          (branch/child-branch-id (:branch ctx) relative-id))
        popover-id child-branch-id                          ; just use child-branch as popover-id
        should-branch @(r/fmap (r/comp some? blank->nil :link/tx-fn) link-ref)
        btn-props (-> props
                      (dissoc :route :tooltip ::redirect)
                      (assoc :on-click (r/partial runtime/dispatch! (:peer ctx)
                                                  (if should-branch
                                                    (open-branched-popover! (:peer ctx) (:branch ctx) child-branch-id popover-id (:route props))
                                                    (actions/open-popover (:branch ctx) popover-id))))
                      ; use twbs btn coloring but not "btn" itself
                      (update :class css "btn-default"))]
    [wrap-with-tooltip popover-id ctx props
     [with-keychord
      "esc" #(do (js/console.warn "esc") (if should-branch
                                           (cancel! popover-id child-branch-id ctx)
                                           (close! popover-id ctx)))
      [re-com/popover-anchor-wrapper
       :showing? (show-popover? popover-id ctx)
       :position :below-center
       :anchor [:button btn-props [:span (str label "▾")]]
       :popover [re-com/popover-content-wrapper
                 :no-clip? true
                 :body [:div.hyperfiddle-popover-body       ; wrpaper helps with popover max-width, hard to layout without this
                        (if should-branch
                          [branched-popover-body (:route props) popover-id child-branch-id link-ref ctx props]
                          [non-branched-popover-body (:route props) popover-id ctx])]]]]]))
