(ns hyperfiddle.ui.popover
  (:require
    [cats.monad.either :as either]
    [cats.core :as cats :refer [mlet return]]
    [contrib.css :refer [css]]
    [contrib.ct :refer [unwrap]]
    [contrib.eval :as eval]
    [contrib.keypress :refer [with-keychord]]
    [contrib.reactive :as r]
    [contrib.pprint :refer [pprint-str]]
    [contrib.string :refer [blank->nil]]
    [contrib.try$ :refer [try-either either->promise]]
    [contrib.ui.tooltip :refer [tooltip tooltip-props]]
    [hypercrud.browser.base :as base]
    [hypercrud.browser.context :as context]
    [hyperfiddle.actions :as actions]
    [hyperfiddle.api]
    [hyperfiddle.branch :as branch]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.security.client :as security]
    [hyperfiddle.ui.error :as ui-error]
    [hyperfiddle.ui.iframe :as iframe]
    [promesa.core :as p]
    [re-com.core :as re-com]
    [taoensso.timbre :as timbre]))


(defn- run-txfn! [ctx props]
  (-> (p/resolved (context/link-tx ctx))
      (p/then
        (fn [user-txfn]
          (runtime/dispatch! (:peer ctx) [:txfn (context/link-class ctx) (context/a ctx)])
          (try
            (let [result (if (contains? (methods hyperfiddle.api/txfn) user-txfn) ; legacy
                           (let [[e a v] (context/eav ctx)]
                             (hyperfiddle.api/txfn user-txfn e a v ctx))
                           (hyperfiddle.api/tx ctx (context/eav ctx) props))]
              ; txfn may be sync or async
              (if (p/promise? result)
                result
                (p/resolved result)))
            (catch js/Error e (p/rejected e)))))))

(defn- stage! [popover-id relative-branch-id ctx r-popover-data props]
  (-> (run-txfn! ctx props)
      (p/then (fn [tx]
                (let [tx-groups {(or (hypercrud.browser.context/dbname ctx) "$") ; https://github.com/hyperfiddle/hyperfiddle/issues/816
                                 tx}
                      on-start [(actions/close-popover (:branch ctx) popover-id)]
                      child-branch-id (branch/child-branch-id (:branch ctx) relative-branch-id)]
                  (cond-> (runtime/commit-branch (:peer ctx) child-branch-id tx-groups on-start)
                    (::redirect props) (p/then (fn [_] (runtime/set-route (:peer ctx) (:branch ctx) ((::redirect props) @r-popover-data))))))))
      (p/catch (fn [e]
                 ; todo something better with these exceptions (could be user error)
                 (timbre/error e)
                 (js/alert (cond-> (ex-message e)
                             (ex-data e) (str "\n" (pprint-str (ex-data e)))))))))

(defn- cancel! [rt parent-branch-id relative-branch-id popover-id]
  (runtime/dispatch! rt (fn [dispatch! get-state]
                          (let [child-branch-id (branch/child-branch-id parent-branch-id relative-branch-id)]
                            (dispatch!
                              (apply actions/batch
                                     (actions/close-popover parent-branch-id popover-id)
                                     (actions/discard-partition get-state child-branch-id)))))))

(defn- open-branched-popover! [rt parent-branch-id relative-branch-id route popover-id]
  (runtime/dispatch! rt (fn [dispatch! get-state]
                          (let [child-branch-id (branch/child-branch-id parent-branch-id relative-branch-id)]
                            (dispatch! (actions/batch [:create-partition child-branch-id]
                                                      [:partition-route child-branch-id route]))
                            (-> (actions/bootstrap-data rt child-branch-id actions/LEVEL-GLOBAL-BASIS)
                                (p/finally (fn [] (dispatch! (actions/open-popover parent-branch-id popover-id)))))))))

(defn- wrap-with-tooltip [popover-id ctx props child]
  ; omit the formula tooltip when popover is open
  (if (runtime/popover-is-open? (:peer ctx) (:branch ctx) popover-id)
    child
    [tooltip (tooltip-props (:tooltip props)) child]))

(defn- disabled? [link-ref ctx]
  (condp some @(r/fmap :link/class link-ref)
    #{:hf/new} nil #_(not @(r/track security/can-create? ctx)) ; flag
    #{:hf/remove} (if (let [[_ a _] @(:hypercrud.browser/eav ctx)] a)
                    (if-let [ctx (:hypercrud.browser/parent ctx)] (not @(r/track security/writable-entity? ctx))) ; check logic
                    (not @(r/track security/writable-entity? ctx)))
    ; else we don't know the semantics, just nil out
    nil))

(defn run-effect! [ctx props]
  (-> (run-txfn! ctx props)
      (p/then
        (fn [tx]
          (cond-> (runtime/with-tx (:peer ctx) (:branch ctx) (context/dbname ctx) tx)
            (::redirect props) (p/then (fn [_] (runtime/set-route (:peer ctx) (:branch ctx) ((::redirect props) nil)))))))
      (p/catch (fn [e]
                 ; todo something better with these exceptions (could be user error)
                 (timbre/error e)
                 (js/alert (cond-> (ex-message e)
                             (ex-data e) (str "\n" (pprint-str (ex-data e)))))))))

(defn ^:export effect-cmp [ctx link-ref props label]
  (let [link-ctx (-> (mlet [ctx (context/refocus-to-link+ ctx link-ref)
                            args (context/build-args+ ctx @link-ref)] ; not sure what args would be in this case
                       (return (context/occlude-eav ctx args))) ; guessing we are occluding v to nil?
                     (either/branch
                       (fn [e] nil)                         ; wtf how does anything work
                       identity))
        props (-> props
                  (assoc :on-click (r/partial run-effect! link-ctx props))
                  (update :class css "hyperfiddle"
                          ; use twbs btn coloring but not "btn" itself
                          (if-not (contains? (methods hyperfiddle.api/tx)
                                             (context/link-tx link-ctx))
                            "btn-outline-danger"
                            "btn-warning"))
                  (update :disabled #(or % (disabled? link-ref link-ctx))))]
    [:button (select-keys props [:class :style :disabled :on-click])
     [:span (str label "!")]]))

(defn- popover-cmp-impl [ctx props & body-children]
  [wrap-with-tooltip (::popover-id props) ctx (select-keys props [:class :on-click :style :disabled :tooltip])
   [with-keychord
    "esc" #(do (js/console.warn "esc") ((::close-popover props) (::popover-id props)))
    [re-com/popover-anchor-wrapper
     :showing? (r/track runtime/popover-is-open? (:peer ctx) (:branch ctx) (::popover-id props))
     :position :below-center
     :anchor [:button (-> props
                          ;(dissoc :route :tooltip ::redirect)
                          (select-keys [:class :style :disabled])
                          ; use twbs btn coloring but not "btn" itself
                          (update :class css "btn-default")
                          (assoc :on-click (r/partial (::open-popover props) (::popover-id props))))
              [:span (str (::label props) "▾")]]
     :popover [re-com/popover-content-wrapper
               :no-clip? true
               ; wrapper helps with popover max-width, hard to layout without this
               :body (into [:div.hyperfiddle-popover-body] body-children)]]]])

(defn- branched-popover-body-cmp [relative-child-branch-id ctx {:keys [::popover-id] :as props}]
  (-> (context/clean ctx)                                   ; hack clean for block level errors
      (context/branch+ relative-child-branch-id)
      (either/branch
        (fn [e]
          [:<>
           [ui-error/error-block e]
           [:div.hyperfiddle-popover-actions
            [:button {:disabled true} "stage"]
            [:button {:on-click #(cancel! (:peer ctx) (:branch ctx) relative-child-branch-id popover-id)} "cancel"]]])
        (fn [branched-ctx]
          [:<>
           (let [branched-ctx (assoc branched-ctx :hyperfiddle.ui/error-with-stage? true)]
             [iframe/iframe-cmp-impl                        ; cycle
              (runtime/get-route (:peer ctx) (:branch branched-ctx))
              (r/partial runtime/set-route (:peer ctx) (:branch branched-ctx))
              branched-ctx])
           [:div.hyperfiddle-popover-actions
            (let [route (runtime/get-route (:peer ctx) (:branch branched-ctx))
                  +popover-ctx-post (base/browse-route+ branched-ctx route) ; todo browse once
                  r-popover-data (r/>>= :hypercrud.browser/result +popover-ctx-post) ; focus the fiddle at least then call @(context/data) ?
                  popover-invalid (->> +popover-ctx-post (unwrap (constantly nil)) context/tree-invalid?)]
              [:button {:on-click #(stage! popover-id relative-child-branch-id ctx r-popover-data props)
                        :disabled popover-invalid} "stage"])
            [:button {:on-click #(cancel! (:peer ctx) (:branch ctx) relative-child-branch-id popover-id)} "cancel"]]]))))

(defn- branched-popover-cmp [relative-child-branch-id ctx props label]
  [popover-cmp-impl ctx (assoc props
                          ::label label
                          ::open-popover (r/partial open-branched-popover! (:peer ctx) (:branch ctx) relative-child-branch-id (:route props))
                          ::close-popover (r/partial cancel! (:peer ctx) (:branch ctx) relative-child-branch-id))
   ; body-cmp NOT inlined for perf
   [branched-popover-body-cmp relative-child-branch-id ctx props]])

(defn- unbranched-popover-body-cmp [relative-child-branch-id ctx props]
  [:<>
   [iframe/iframe-cmp                                       ; cycle
    (:route props)
    (context/clean ctx)                                     ; hack clean for block level errors
    {::iframe/relative-child-branch-id relative-child-branch-id}]
   [:button {:on-click #(runtime/close-popover (:peer ctx) (:branch ctx) (::popover-id props))} "close"]])

(defn- unbranched-popover-cmp [relative-child-branch-id ctx props label]
  [popover-cmp-impl ctx (assoc props
                          ::label label
                          ::open-popover (r/partial runtime/open-popover (:peer ctx) (:branch ctx))
                          ::close-popover (r/partial runtime/close-popover (:peer ctx) (:branch ctx)))
   ; body-cmp NOT inlined for perf
   [unbranched-popover-body-cmp relative-child-branch-id ctx props]])

(defn ^:export popover-cmp [ctx link-ref props label]
  (let [+route-and-ctx (context/refocus-build-route-and-occlude+ ctx link-ref) ; Can fail if formula dependency isn't satisfied
        link-ctx (either/branch
                   +route-and-ctx
                   (constantly nil)                         ; how can this safely be nil
                   first)
        props (-> (cats/fmap second +route-and-ctx)
                  (hyperfiddle.ui/validated-route-tooltip-props link-ref link-ctx props)
                  (update :class css "hyperfiddle")
                  (update :disabled #(or % (disabled? link-ref link-ctx))))
        ; try to auto-generate branch/popover-id from the product of:
        ; - link's :db/id
        ; - route
        ; - visual-ctx's data & path (where this popover is being drawn NOT its dependencies)
        visual-eav (context/eav ctx)
        relative-child-branch-id (context/build-child-branch-relative-id link-ctx
                                                                         @(r/fmap :db/id link-ref)
                                                                         (:route props)
                                                                         ; visual-a de-dupes various identity columns which show exactly the same link
                                                                         visual-eav)
        should-branch @(r/fmap (r/comp some? blank->nil :link/tx-fn) link-ref)]
    (if should-branch
      (let [props (assoc props
                    ; just use child-branch as popover-id
                    ::popover-id (branch/child-branch-id (:branch ctx) relative-child-branch-id))]
        [branched-popover-cmp relative-child-branch-id link-ctx props label])
      (let [props (assoc props
                    ; just use child-branch as popover-id
                    ::popover-id (branch/child-branch-id (:branch ctx) relative-child-branch-id))]
        [unbranched-popover-cmp relative-child-branch-id link-ctx props label]))))
