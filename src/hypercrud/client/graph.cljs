(ns hypercrud.client.graph
  (:require [goog.Uri]
            [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx]
            [hypercrud.client.util :as util]))


(defprotocol GraphPrivate
  (touch! [this pulled-trees-map']))

;; Really we just want to be able to serialize graphs for the wire; this is a quick hack
;; because we aren't sure what we want to do about the schema which is part of the graph and the client
(defprotocol GraphSSR
  (named-queries* [this])
  (pulled-trees-map* [this]))


;; all must be memoizied
(defn ->pulled-trees [pulled-trees-map]
  (apply concat (vals pulled-trees-map)))

(defn ->statements [schema pulled-trees-map]
  (mapcat #(tx/pulled-tree-to-statements schema %) (->pulled-trees pulled-trees-map)))

(defn ->resultsets [pulled-trees-map]
  (util/map-values #(map :db/id %) pulled-trees-map))


(deftype Graph [schema named-queries t local-statements ^:mutable pulled-trees-map]
  hc/Graph
  (select [this named-query]
    (get (->resultsets pulled-trees-map) (get named-queries named-query)))


  (entity [this eid]
    (tx/apply-statements-to-entity
      schema
      (concat (->statements schema pulled-trees-map) local-statements)
      {:db/id eid}))


  (with [this more-statements]
    (Graph. schema named-queries t (concat local-statements more-statements) pulled-trees-map))


  IHash
  (-hash [this]
    (hash (map hash [schema named-queries t local-statements])))


  IEquiv
  (-equiv [this other]
    (= (hash this) (hash other)))


  GraphPrivate
  (touch! [this pulled-trees-map']
    (set! pulled-trees-map pulled-trees-map'))

  GraphSSR
  (named-queries* [this] named-queries)
  (pulled-trees-map* [this] pulled-trees-map))
