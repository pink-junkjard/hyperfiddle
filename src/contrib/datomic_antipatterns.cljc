(ns contrib.datomic-antipatterns
  (:require
    [clojure.walk :as walk]))


; This is a graveyard of bad ideas.
; It is helpful to review them the next time we feel a similar need.

; rename: walk-pulled-tree (respecting component)
(defn walk-entity [schema f entity]                         ; Walks entity components, applying f to each component, dehydrating non-components
  (->> (f entity)
       (mapv
         (fn [[a v]]
           (let [{:keys [:db/cardinality :db/valueType :db/isComponent]} (get schema a) ; really just for component, the rest could be polymorphic
                 v (if-not (= (:db/ident valueType) :db.type/ref) ; dbid absent from schema, its fine
                     v
                     (if isComponent
                       (case (:db/ident cardinality)        ; Walk component (go deeper)
                         :db.cardinality/one (walk-entity schema f v)
                         :db.cardinality/many (mapv #(walk-entity schema f %) v))

                       (case (:db/ident cardinality)        ; Dehydrate non-component
                         :db.cardinality/one (select-keys v [:db/id])
                         :db.cardinality/many (mapv #(select-keys % [:db/id]) v))))]
             [a v])))
       (into {})))

; don't know what this is
(defn ^:legacy walk-pulled-tree [schema f tree]                      ; don't actually need schema for anything but component which is ignored here
  (walk/postwalk
    (fn [o] (if (map? o) (f o) o))
    tree))

(defn prewalk-pulled-tree [schema f tree]
  ; Failed experiment for datomic pull parsing
  (->> (f tree)
       (mapv
         (fn [[a v]]
           (let [{{cardinality :db/ident} :db/cardinality
                  {valueType :db/ident} :db/valueType} (get schema a)
                 v (if-not (= :db.type/ref valueType)       ; dbid absent from schema, its fine
                     v
                     (case cardinality
                       :db.cardinality/one (prewalk-pulled-tree schema f v)
                       :db.cardinality/many (mapv #(prewalk-pulled-tree schema f %) v)))]
             [a v])))
       (into {})))

(comment
  (prewalk-pulled-tree schema #(do (println %) %) {:db/id 10 :db/ident :yo})
  (prewalk-pulled-tree schema #(do (println %) %) {:db/id 17592186046209,
                                                   :db/ident :shirt-size/womens-medium,
                                                   :hyperfiddle/owners [#uuid "acd054a8-4e36-4d6c-a9ec-95bdc47f0d39"],
                                                   :reg/gender {:db/id 17592186046204}})
  ;(prewalk-pulled-tree schema distinct pulled-tree-1)
  )

; Antipattern because you really only want to descend pulls. You should unwind to the top
; and descend while comparing to a resultpath, or something.
(defn pullpath-unwind-while "Find oldest ancestor matching pred.
  Hint: Pred probably closes over schema."
  [f? pullpath]
  #_(drop-while f? (reverse pullpath))
  (loop [[a & as :as here] (reverse pullpath)]              ; pullpath is guaranteed to align with pullshape
    (if (and a (f? a))
      (recur as)
      here)))
