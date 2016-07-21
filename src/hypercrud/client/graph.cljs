(ns hypercrud.client.graph
  (:require [goog.Uri]
            [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx]
            [hypercrud.client.util :as util]))


(defprotocol GraphPrivate
  (pulled-trees-map* [this]))


(deftype Graph [schema named-queries pulled-trees-map
                statements resultsets                       ; precomputed for speed
                local-statements]
  hc/Graph
  (select [this named-query]
    (get resultsets (get named-queries named-query)))


  (entity [this eid]
    (tx/apply-statements-to-entity
      schema
      (concat statements local-statements)
      {:db/id eid}))


  (with [this more-statements]
    (Graph.
      schema named-queries pulled-trees-map statements resultsets (concat local-statements more-statements)))


  IHash
  (-hash [this]
    (hash (map hash [schema statements resultsets local-statements])))


  IEquiv
  (-equiv [this other]
    (= (hash this) (hash other)))

  GraphPrivate
  (pulled-trees-map* [this] pulled-trees-map))


(defn graph [schema named-queries pulled-trees-map]
  (let [pulled-trees (apply concat (vals pulled-trees-map)) ;mash query results together
        statements (mapcat #(tx/pulled-tree-to-statements schema %) pulled-trees)
        resultsets (util/map-values #(map :db/id %) pulled-trees-map)]
    (->Graph schema named-queries pulled-trees-map statements resultsets [])))
