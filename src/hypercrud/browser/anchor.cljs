(ns hypercrud.browser.anchor
  (:require [cats.core :refer [mlet extract return]]
            [cats.monad.exception :as exception :refer [try-on success failure success?]]
            [clojure.set :as set]
            [hypercrud.browser.auto-anchor-formula :refer [auto-entity-dbid]]
            [hypercrud.browser.connection-color :as connection-color]
            [hypercrud.client.core :as hc]
            [hypercrud.compile.eval :as eval :refer [eval-str']]
            [hypercrud.form.q-util :as q-util]
            [hypercrud.runtime.state.actions :as actions]   ; todo bad dep
            [hypercrud.util.branch :as branch]
            [promesa.core :as p]))


(defn safe-run-user-code-str' [code-str & args]
  (if-let [code-str (eval/validate-user-code-str code-str)]
    (mlet [user-fn (eval-str' code-str)]
      (if user-fn
        (exception/try-on (apply user-fn args))
        (return nil)))))

(defn validated-route' [link route]
  ; We specifically hydrate this deep just so we can validate anchors like this.
  (case (:request/type link)
    :query (mlet [q (success (q-util/safe-parse-query-validated link))]
             (let [have (set (keys (into {} (remove (comp nil? val) (:query-params route)))))
                   need (set (q-util/parse-param-holes q))
                   missing (set/difference need have)]
               (if (empty? missing)
                 (success route)
                 (failure missing "missing query params"))))
    :entity (if (not= nil (-> route :query-params :entity)) ; add logic for a
              (success route)
              (failure #{:entity} "missing query params"))
    :blank (success route)
    (success route) #_"wtf???  \"{:hypercrud/owner {:database/domain nil, :database/ident \"hyperfiddle\"}, :db/id #DbId[[:link/ident :hyperfiddle/new-page-popover] 17592186045422]}\"   "))

(defn ^:export #_ "all fiddles render fn" build-anchor-route-unvalidated' [anchor param-ctx]
  (mlet [query-params (if-let [code-str (eval/validate-user-code-str (:anchor/formula anchor))]
                        (safe-run-user-code-str' code-str param-ctx)
                        (success nil))]
    (let [route {:domain (-> anchor :anchor/link :hypercrud/owner :database/domain)
                 :project (-> anchor :anchor/link :hypercrud/owner :database/ident)
                 :link-dbid (-> anchor :anchor/link :db/id)
                 :query-params query-params}]
      (success route))))

(defn ^:export build-anchor-route' [anchor param-ctx]
  ;(assert project)                                         ; be safe - maybe constructing it now
  (mlet [route (build-anchor-route-unvalidated' anchor param-ctx)]
    (validated-route' (:anchor/link anchor) route)))

(defn anchor-tooltip [link route' param-ctx]
  (case (:display-mode param-ctx)
    :xray (if (success? route')
            [nil (pr-str (:query-params @route'))]
            [:warning (pr-str (extract route'))])
    nil))

(defn build-anchor-props-raw [route' link param-ctx]        ; param-ctx is for display-mode
  ; doesn't handle tx-fn - meant for the self-link. Weird and prob bad.
  {:route (exception/extract route' nil)
   :style {:color (connection-color/connection-color (-> link :hypercrud/owner :db/id :id))}
   :tooltip (anchor-tooltip link route' param-ctx)
   :class (if-not (success? route') "invalid")})

(defn anchor-branch-logic [anchor param-ctx]
  (if (:anchor/managed? anchor)
    (if-let [db (:db param-ctx)]
      (let [branch (branch/encode-branch-child (.-branch db) (:id (auto-entity-dbid param-ctx)))]
        (-> param-ctx
            ; if we are an index link, what are we forking? Provide a binding
            (assoc-in [:branches (.-conn-id db)] branch)
            (update :db #(hc/db (:peer param-ctx) (.-conn-id %) branch))))
      ; Inform user via tooltip that we can't branch an index link because there is no db in scope. Explicitly set db in user bindings.
      param-ctx)
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
                       (mlet [user-fn (eval-str' code-str)] (exception/try-on (user-fn param-ctx)))
                       (success true))
                     (exception/extract true))
        route' (if (:anchor/link anchor) (build-anchor-route' anchor param-ctx #_"links & routes have nothing to do with branches"))
        anchor-props-route (if route' (build-anchor-props-raw route' (:anchor/link anchor) param-ctx))
        param-ctx (anchor-branch-logic anchor param-ctx)
        anchor-props-txfn (if-let [user-txfn (some-> (eval/validate-user-code-str (:anchor/tx-fn anchor)) eval-str' (exception/extract nil))]
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
        anchor-props-popover (if-let [route (and (:anchor/managed? anchor) (exception/extract route' nil))]
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
