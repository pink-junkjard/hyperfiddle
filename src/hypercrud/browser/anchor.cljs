(ns hypercrud.browser.anchor
  (:require [cats.core :as cats :refer [mlet return]]
            [cats.monad.either :as either]
            [cats.monad.exception :as exception]
            [hypercrud.browser.auto-anchor-formula :refer [auto-entity-dbid]]
            [hypercrud.browser.base :as base]
            [hypercrud.browser.connection-color :as connection-color]
            [hypercrud.client.core :as hc]
            [hypercrud.compile.eval :as eval :refer [eval-str']]
            [hypercrud.runtime.state.actions :as actions]   ; todo bad dep
            [hypercrud.util.branch :as branch]
            [hypercrud.util.monad :refer [exception->either]]
            [promesa.core :as p]))


(defn safe-run-user-code-str' [code-str & args]
  (if-let [code-str (eval/validate-user-code-str code-str)]
    (mlet [user-fn (eval-str' code-str)]
      (if user-fn
        (-> (exception/try-on (apply user-fn args))
            exception->either)
        (return nil)))
    (either/right nil)))

#_"all fiddles render fn"
(defn ^:export build-anchor-route-unvalidated' [anchor param-ctx]
  (mlet [query-params (safe-run-user-code-str' (:anchor/formula anchor) param-ctx)]
    (return {:domain (-> anchor :anchor/link :hypercrud/owner :database/domain)
             :project (-> anchor :anchor/link :hypercrud/owner :database/ident)
             :link-dbid (-> anchor :anchor/link :db/id)
             :query-params query-params})))

(defn ^:export build-anchor-route' [anchor param-ctx]
  ;(assert project)                                         ; be safe - maybe constructing it now
  (mlet [route (build-anchor-route-unvalidated' anchor param-ctx)
         ; validate the route by seeing if the request can be built the same as the browser
         _ (base/request-for-link (:anchor/link anchor) (:query-params route) param-ctx)]
    (return route)))

(defn anchor-tooltip [route' param-ctx]
  (case (:display-mode param-ctx)
    :xray (-> route'
              (either/branch
                (fn [e] [:warning (pr-str e)])
                (fn [route] [nil (pr-str (:query-params route))])))
    nil))

(defn build-anchor-props-raw [route' link param-ctx]        ; param-ctx is for display-mode
  ; doesn't handle tx-fn - meant for the self-link. Weird and prob bad.
  {:route (-> route'
              (cats/mplus (either/right nil))
              (cats/extract))
   :style {:color (connection-color/connection-color (-> link :hypercrud/owner :db/id :id))}
   :tooltip (anchor-tooltip route' param-ctx)
   :class (if (either/left? route') "invalid")})

(defn anchor-branch-logic [anchor param-ctx]
  (if (:anchor/managed? anchor)
    (if-let [db (:db param-ctx)]
      (let [branch (branch/encode-branch-child (.-branch db) (:id (auto-entity-dbid param-ctx)))]
        (-> param-ctx
            ; if we are an index link, what are we forking? Provide a binding
            (assoc-in [:branches (.-conn-id db)] branch)
            (update :db #(hc/db (:peer param-ctx) (.-conn-id %) branch))))
      (do
        (js/console.warn "You are attempting to branch an index-link. We can't deduce the :db to branch, you must explicitly set it in user bindings.")
        param-ctx))
    param-ctx))

; if this is driven by anchor, and not route, it needs memoized.
; the route is a fn of the formulas and the formulas can have effects
; which have to be run only once.
(defn build-anchor-props [anchor param-ctx]
  ; Draw as much as possible even in the presence of errors, still draw the link, collect all errors in a tooltip.
  ; Error states:
  ; - no route
  ; - invalid route
  ; - broken user formula
  ; - broken user txfn
  ; - broken user visible fn
  ; If these fns are ommitted (nil), its not an error.
  (let [visible? (-> (if-let [code-str (eval/validate-user-code-str (:anchor/visible? anchor))]
                       (mlet [user-fn (eval-str' code-str)]
                         (-> (exception/try-on (user-fn param-ctx))
                             exception->either))
                       (either/right true))
                     (cats/mplus (either/right true))
                     (cats/extract))
        route' (if (:anchor/link anchor) (build-anchor-route' anchor param-ctx #_"links & routes have nothing to do with branches"))
        anchor-props-route (if route' (build-anchor-props-raw route' (:anchor/link anchor) param-ctx))
        param-ctx (anchor-branch-logic anchor param-ctx)
        anchor-props-txfn (if-let [user-txfn (some-> (eval/validate-user-code-str (:anchor/tx-fn anchor))
                                                     eval-str'
                                                     (cats/mplus (either/right nil))
                                                     (cats/extract))]
                            (do
                              ;(assert (-contains-key? param-ctx :db))
                              ; do we need to hydrate any dependencies in this chain?
                              {:txfns {:stage (fn []
                                                (p/promise (fn [resolve reject]
                                                             (let [swap-fn (fn [tx-from-modal]
                                                                             (let [result (let [result (user-txfn param-ctx tx-from-modal)]
                                                                                            ; txfn may be sync or async
                                                                                            (if-not (p/promise? result) (p/resolved result) result))]
                                                                               ; let the caller of this :stage fn know the result
                                                                               (p/branch result
                                                                                         (fn [result] (resolve nil))
                                                                                         (fn [why]
                                                                                           (reject why)
                                                                                           (js/console.error why)))

                                                                               ; return the result to the action
                                                                               result))]
                                                               ((:dispatch! param-ctx) (actions/stage-popover (.-conn-id (:db param-ctx)) (.-branch (:db param-ctx)) swap-fn))))))
                                       :cancel #((:dispatch! param-ctx) (actions/discard (.-conn-id (:db param-ctx)) (.-branch (:db param-ctx))))}}))

        ; the whole point of popovers is managed branches
        anchor-props-popover (if-let [route (and (:anchor/managed? anchor) (either/right? route') (cats/extract route'))]
                               ; If no route, there's nothing to draw, and the anchor tooltip shows the error.
                               {:popover (fn []
                                           [:div
                                            (case (:display-mode param-ctx)
                                              :xray [(:navigate-cmp param-ctx) anchor-props-route "self"]
                                              nil)
                                            [hypercrud.browser.core/safe-ui' ; cycle
                                             route          ; draw the branch
                                             (dissoc param-ctx :result :db :find-element :entity :attribute :value :layout :field)]])})
        anchor-props-hidden {:hidden (not visible?)}]
    (merge anchor-props-route anchor-props-txfn anchor-props-popover anchor-props-hidden)))
