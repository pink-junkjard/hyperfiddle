(ns hypercrud.client.graph
  (:require [goog.Uri]
            [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx]))


(defprotocol GraphPrivate
  (statements* [this])
  (resultsets* [this]))


(deftype Graph [schema statements resultsets local-statements]
  hc/Graph
  (select [this named-query]
    (get resultsets named-query))


  (entity [this eid]
    (tx/apply-statements-to-entity
      schema
      (concat statements local-statements)
      {:db/id eid}))


  (with [this more-statements]
    (Graph.
      schema statements resultsets (concat local-statements more-statements)))


  IHash
  (-hash [this]
    (hash (map hash [schema statements resultsets local-statements])))


  IEquiv
  (-equiv [this other]
    (= (hash this) (hash other)))

  GraphPrivate
  (statements* [this] statements)
  (resultsets* [this] resultsets))
