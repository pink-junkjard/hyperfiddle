(ns hypercrud.browser.auto-anchor-formula
  (:require-macros [hypercrud.util.template :as template])
  (:require [hypercrud.types.DbId :refer [->DbId]]
            [hypercrud.util.core :as util]
            [hypercrud.util.vedn :as vedn]))


(defn auto-entity-dbid-from-stage [conn-id branch param-ctx]
  (let [stage-val (-> param-ctx :peer .-state-atom deref :stage)  ; This line is not allowed, it breaks determinism.
        branch-val (get-in stage-val [conn-id branch])
        id (-> (or branch-val "nil stage")
               hash js/Math.abs - str)]
    (->DbId id conn-id)))

(defn deterministic-ident [fe e a v]
  ; Need comment explaining why.
  ; [fe e a v] quad is sufficient to answer "where are we".
  ; Why Db is omitted?
  ; Why value is only inspected in :many for unique hashing?
  (-> (str (-> fe :find-element/name) "."
           (-> e :db/id :id) "."
           (-> a :attribute/ident) "."
           (case (get-in a [:attribute/cardinality :db/ident])
             :db.cardinality/one nil
             :db.cardinality/many (hash (into #{} (mapv :db/id v))) ; todo scalar
             nil nil #_":db/id has a faked attribute with no cardinality, need more thought to make elegant"))
      hash js/Math.abs - str
      ))

(defn auto-entity-dbid [ctx & [conn-id]]
  (->DbId (deterministic-ident
            (-> ctx :find-element)
            (-> ctx :entity)
            (-> ctx :attribute)
            (-> ctx :value))
          (or conn-id (-> ctx :entity :db/id :conn-id))))

(def auto-formula-lookup
  (let [fe-no-create (->> (template/load-resource "auto-formula/fe-no-create.vedn")
                          (vedn/read-string)
                          (util/map-keys #(assoc % :fe true :c? false)))
        fe-create (->> (template/load-resource "auto-formula/fe-create.vedn")
                       (vedn/read-string)
                       (util/map-keys #(assoc % :fe true :c? true)))
        ; no fe = index or relation links
        no-fe {{:fe false :c? false :d? true :a false} nil
               {:fe false :c? false :d? true :a true} nil
               {:fe false :c? false :d? false :a false} nil
               {:fe false :c? false :d? false :a true} nil

               {:fe false :c? true :d? true :a false} nil
               {:fe false :c? true :d? true :a true} nil
               {:fe false :c? true :d? false :a false} nil
               {:fe false :c? true :d? false :a true} nil}]
    (merge fe-create fe-no-create no-fe)))

; anchors MUST have a find-element, if they have an attribute or are create? or repeating?
;(assert (not (and (not fe) (or a create? (:anchor/repeating? anchor)))) "missing find-element")
(defn auto-formula [anchor]                                 ; what about long-coersion?
  (get auto-formula-lookup
       {:fe (not (nil? (:anchor/find-element anchor)))
        :c? (or (:anchor/create? anchor) false)
        :d? (or (:anchor/repeating? anchor) false)
        :a (not (nil? (:anchor/attribute anchor)))}))
