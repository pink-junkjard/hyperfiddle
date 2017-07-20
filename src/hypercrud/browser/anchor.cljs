(ns hypercrud.browser.anchor
  (:require [cats.core :as cats :refer [mlet alet return]]
            [cats.monad.exception :as exception :refer [try-on]]
            [clojure.set :as set]
            [hypercrud.browser.auto-anchor-formula :refer [auto-entity-dbid]]
            [hypercrud.browser.connection-color :as connection-color]
            [hypercrud.client.core :as hc]
            [hypercrud.compile.eval :refer [eval-str']]
            [hypercrud.form.q-util :as q-util]
            [hypercrud.runtime.state.actions :as actions]   ; todo bad dep
            [hypercrud.util.branch :as branch]
            [promesa.core :as p]))


(defn safe-run-user-code-str' [code-str param-ctx]
  (if (empty? code-str)
    (exception/success nil)
    (mlet [user-fn (eval-str' code-str)] (try-on (user-fn param-ctx)))))

(defn build-anchor-route'
  ([domain project link-dbid formula-str param-ctx]
    ;(assert project)                                         ; safe - maybe constructing it now
   (mlet [query-params (safe-run-user-code-str' formula-str param-ctx)]
     (return {:domain domain
              :project project
              :link-dbid link-dbid #_:id
              :query-params query-params})))
  ([link formula-str param-ctx]
   (build-anchor-route'
     (-> link :hypercrud/owner :database/domain)
     (-> link :hypercrud/owner :database/ident)
     (-> link :db/id)
     formula-str
     param-ctx))
  ([anchor param-ctx]
   (build-anchor-route' (:anchor/link anchor) (:anchor/formula anchor) param-ctx)))

(defn holes-filled? [hole-names query-params-map]
  (set/subset? (set hole-names) (set (keys (into {} (remove (comp nil? val) query-params-map))))))

(defn anchor-valid? [link route']                           ; could return monad to say why
  ; We specifically hydrate this deep just so we can validate anchors like this.
  (case (:request/type link)
    :query (some-> link
                   q-util/safe-parse-query-validated
                   q-util/parse-param-holes
                   (holes-filled? (:query-params (exception/extract route' nil))))
    :entity (not= nil (-> (exception/extract route' nil) :query-params :entity))     ; add logic for a
    :blank true
    true))

(defn anchor-valid?' [anchor route]
  (anchor-valid? (:anchor/link anchor) route))

(defn anchor-tooltip [link route' param-ctx]
  (case (:display-mode param-ctx)
    :xray (if (anchor-valid? link route')
            ; can do better error reporting here.
            [nil (pr-str (:query-params (exception/extract route' nil)))]
            [:warning (pr-str (:query-params (exception/extract route' nil)))])
    nil))

(defn build-anchor-props-raw [route' link param-ctx]        ; param-ctx is for display-mode
  ; doesn't handle tx-fn - meant for the self-link. Weird and prob bad.
  {:route (exception/extract route' nil)
   :style {:color (connection-color/connection-color (-> link :hypercrud/owner :db/id :id))}
   :tooltip (anchor-tooltip link route' param-ctx)
   :class (if-not (anchor-valid? link route') "invalid")})

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
  (let [maybe-txfn' (if-let [user-fn-str (:anchor/tx-fn anchor)] (eval-str' user-fn-str))
        maybe-visible' (if-let [user-fn-str (:anchor/visible? anchor)]
                         (if-not (empty? user-fn-str)
                           (mlet [user-fn (eval-str' user-fn-str)] (try-on (user-fn param-ctx)))))
        route' (if (:anchor/link anchor) (build-anchor-route' anchor param-ctx #_"links & routes have nothing to do with branches"))]
    (let [anchor-props-route (if route' (build-anchor-props-raw route' (:anchor/link anchor) param-ctx))

          param-ctx (anchor-branch-logic anchor param-ctx)
          param-ctx (assoc param-ctx :link-owner (-> anchor :anchor/link :hypercrud/owner)) #_"txfn may need this"

          anchor-props-txfn (if-let [user-txfn (and maybe-txfn' (exception/extract maybe-txfn' nil))] ; better also be managed!
                              ; do we need to hydrate any dependencies in this chain?
                              {:txfns {:stage (fn []
                                                (p/promise (fn [resolve reject]
                                                             (let [swap-fn (fn [get-tx-from-modal]
                                                                             (let [result (let [result (user-txfn param-ctx get-tx-from-modal)]
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
                                       :cancel #((:dispatch! param-ctx) (actions/discard (.-conn-id (:db param-ctx)) (.-branch (:db param-ctx))))}})

          ; the whole point of popovers is managed branches
          anchor-props-popover (if-let [route (and (:anchor/managed? anchor) (exception/extract route' nil))]
                                 ; If no route, there's nothing to draw, and the anchor tooltip shows the error.
                                 {:popover (fn []
                                             [:div
                                              (case (:display-mode param-ctx)
                                                :xray [(:navigate-cmp param-ctx) anchor-props-route "self"]
                                                nil)
                                              [hypercrud.browser.core/safe-ui' ; cycle
                                               route        ; draw the branch
                                               (dissoc param-ctx :result :db :find-element :entity :attribute :value :layout)]])})
          anchor-props-hidden {:hidden (not (if maybe-visible' (exception/extract maybe-visible' true) true))}]
      (merge anchor-props-route anchor-props-txfn anchor-props-popover anchor-props-hidden))))
