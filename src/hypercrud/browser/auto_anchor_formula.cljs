(ns hypercrud.browser.auto-anchor-formula
  (:require [hypercrud.types.DbId :refer [->DbId]]
            [hypercrud.util.core :as util]))


(defn auto-formula [anchor]                                 ; what about long-coersion?
  ; Future improvement:
  ; we already have this info in the runtime param-ctx, so we could delay until formula runtime
  ; and not look at the anchor at all and bypass the formula read-eval.

  ; this is a 3x3 matrix - repeating, entity, attribute. Find element is not part of the matrix.
  ; link-query's always have a find-element, if they have an attribute
  ; link-entity's never do, despite having an attribute.

  ; Fn syntax can go here if we have proper cljs dynamic vars.
  (let [{r :anchor/repeating? e :anchor/find-element a :anchor/attribute} anchor]
    (cond

      ; attr edit
      (and r a)
      (pr-str `(fn [ctx#]
                 (let [attr# (-> ctx# :attribute)]
                   (case (-> ((:schema ctx#) (:attribute/ident attr#)) :attribute/cardinality :db/ident)
                     :db.cardinality/one {:entity (get-in ctx# [:value :db/id])}
                     :db.cardinality/many {:entity (get-in ctx# [:entity :db/id])
                                           :a (:attribute/ident attr#)}))))

      ; attr create (managed, see auto-txfn)
      (and (not r) a)
      ; inherit parent since the fe is never explicitly set by user
      ; it would be more correct to use the FE if we have it, but
      ; that information is guaranteed to be the same?
      (pr-str `(fn [ctx#]
                 (assert (-> ctx# :entity))
                 (assert (-> ctx# :entity :db/id :conn-id))
                 {:entity
                  (->DbId (-> (str (-> ctx# :entity :db/id :id) "."
                                   (-> ctx# :attribute :attribute/ident) "."
                                   (case (-> ((:schema ctx#) (-> ctx# :attribute :attribute/ident)) :attribute/cardinality :db/ident)
                                     :db.cardinality/one nil
                                     :db.cardinality/many (hash (into #{} (mapv :db/id (:value ctx#))))))
                              hash js/Math.abs - str)
                          (-> ctx# :entity :db/id :conn-id))}))

      ; entity edit
      (and r (not a))
      (pr-str `(fn [ctx#]
                 {:entity (get-in ctx# [:entity :db/id])}))

      ; entity create
      ; is it managed or not? We need a connection. Either we got the find-element, or
      ; we are managed. If we're managed, we need an entity in scope, to conjure a connection.
      ; So despite not really needing the value in scope, we need the connection, so we need the value.
      ; This is counter intuitive. It only happens for sys links. Regular links set the linkentity/connection
      ; so don't have this problem.
      ; Mystery deepens: If ur a syslink u better have a conn-id here because autolink inspects the entity connid to manufacture
      ; the right entity connection. If you're an explicit link with a conn set, it doesn't matter what you put here since the server
      ; will ignore this and use the explicit conn. This is only needed to plumb a connection to the autolink logic so it can choose the right connection.
      (and (not r) (not a))
      (pr-str `(fn [ctx#]
                 {:entity
                  (->DbId (-> (str (-> ctx# :entity :db/id :id) "." ".")
                              hash js/Math.abs - str)
                          (or ~(-> e :find-element/connection :db/id :id)
                              (-> ctx# :entity :db/id :conn-id)))}))

      ; naked
      (and (not r) (not e) (not a)) nil

      ; relation edit (this is not really a thing)
      ; If this is a thing, it probably is a query with named params and a custom formula.
      (and r (not e) (not a)) nil

      :else (assert false (str "auto-formula matrix - missing pattern: " (util/pprint-str [r e a]))))))
