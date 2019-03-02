(ns contrib.datascript
  (:require
    [contrib.datomic]
    [contrib.datomic-antipatterns]
    [datascript.core]))


(defmulti f (fn [a tree] a))
(defmethod f :default [_ tree] tree)
(defmethod f :db/valueType [_ tree]
  (assert (= 1 (count tree)))
  (if (= (:db/ident tree) :db.type/ref)
    {:db/valueType (:db/ident tree)}
    nil ; omit other types for datascript
    ))
(defmethod f :db/cardinality [_ tree]
  (assert (= 1 (count tree)))
  {:db/cardinality (:db/ident tree)})
(defmethod f :db/unique [_ tree]
  (assert (= 1 (count tree)))
  {:db/unique (:db/ident tree)})

(defn fix-attribute [attribute]
  (map (partial apply f) attribute))

(comment
  (fix-attribute (first -schema))
  (->> (map fix-attribute -schema)
       )

  (def -schema (.-schema-pulledtree fixtures.ctx/schema))
  (map (fn [[a tree]] (f a tree)) (first -schema))
  (map (partial apply f) (first -schema))
  (map (partial apply f) -schema)

  (map (partial apply f) (.-schema-pulledtree fixtures.ctx/schema))
  )

(defn datomic-schema->datascript-schema [schema-pulledtree]

  (let [ds-schema (->> schema-pulledtree
                       (map a))




        ]
    )



  )

(comment
  (require 'fixtures.domains)
  (datascript.core/empty-db (.-schema-by-attr fixtures.domains/schema))

  )

(defn datascript-from-result [schemas qfind data]
  (-> (contrib.datomic/spread-elements' (fn [schema element coll]
                                          (let [{{db :symbol} :source {pull-pattern :value} :pattern} element]
                                            [db schema element coll])) schemas qfind data)
      (->> (group-by first))                                ; Collect results from same db
      (->> (map (fn [elements-group]
                  (let [[[db] [schema] elements colls] (contrib.data/transpose elements-group)]
                    (-> (datascript.core/empty-db schema)
                        (datascript.core/db-with
                          (contrib.datomic-antipatterns/pulled-tree->statements
                            schema
                            (apply concat colls)))))))))

  #_(let [schema {:human/name {}
                  :human/starships {:db/valueType :db.type/ref
                                    :db/cardinality :db.cardinality/many}
                  :ship/name {}
                  :ship/class {}}
          data [{:human/name "Naomi Nagata"
                 :human/starships [{:db/id -1 :ship/name "Roci" :ship/class :ship.class/fighter}
                                   {:ship/name "Anubis" :ship/class :ship.class/science-vessel}]}
                {:human/name "Amos Burton"
                 :human/starships [-1]}]]
      ))