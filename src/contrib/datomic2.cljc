(ns contrib.datomic2
  (:require
    [contrib.datomic]))


(defn reachable-pullpaths [schema root-pullshape pullpath]
  ; Include the one we are at now? There is an off by one in here
  {:pre [schema #_(satisfies? SchemaIndexedNormalized schema)]}
  ; txfn can be on scalar and it is harmless to allow this.
  (let [pred #(contrib.datomic/cardinality? schema % :db.cardinality/one) #_(ref-one? schema)
        ancestor-path (contrib.datomic/pullpath-unwind-while pred pullpath)
        ancestor-pull (contrib.datomic/pullshape-get-in root-pullshape ancestor-path)]
    (contrib.datomic/pull-traverse schema ancestor-pull pred)))

(defn reachable-attrs [schema root-pullshape pullpath]
  {:pre [#_(satisfies? SchemaIndexedNormalized schema)]}
  (->> (reachable-pullpaths schema root-pullshape pullpath)
       (map last)
       (remove nil?)))
