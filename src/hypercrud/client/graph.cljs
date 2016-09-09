(ns hypercrud.client.graph
  (:require [cljs.pprint :as pprint]
            [goog.Uri]
            [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx]
            [hypercrud.client.util :as util]))


(defprotocol GraphPrivate
  (set-state! [this pulled-trees-map' t']))

;; Really we just want to be able to serialize graphs for the wire; this is a quick hack
;; because we aren't sure what we want to do about the schema which is part of the graph and the client
(defprotocol GraphSSR
  (named-queries* [this])
  (pulled-trees-map* [this]))


(defn ->pulled-trees [pulled-trees-map]
  (apply concat (vals pulled-trees-map)))


(defn ->statements [schema pulled-trees-map]
  (mapcat #(tx/pulled-tree-to-statements schema %) (->pulled-trees pulled-trees-map)))


(defn ->resultsets [pulled-trees-map]
  (util/map-values #(map :db/id %) pulled-trees-map))


(defrecord GraphData [pulled-trees-map resultsets entity-lookup])


(defn ->graph-data [schema pulled-trees-map local-statements]
  (let [statements (->statements schema pulled-trees-map)]
    (GraphData. pulled-trees-map
                (->resultsets pulled-trees-map)
                (tx/build-entity-lookup schema (concat statements local-statements)))))


(deftype Graph [schema named-queries ^:mutable t local-statements ^:mutable graph-data]
  hc/Graph
  (select [this named-query] (hc/select this named-query nil))

  (select [this named-query query-message]
    (assert (contains? named-queries named-query)
            (let [named-query (or query-message named-query)]
              (str "Named-query: " named-query " not found in:\n" (with-out-str (pprint/pprint named-queries)))))
    (get (:resultsets graph-data) (get named-queries named-query)))


  (entity [this eid]
    (assert (not= nil eid) "Cannot request nil entity")
    ; graph should track tempids provided, and only return entities for where the id has been allocated
    ; otherwise we should throw
    (or
      (get (:entity-lookup graph-data) eid)
      (do
        (assert (tx/tempid? eid) (str "Entity not found locally: " eid))
        {:db/id eid})))


  (with [this more-statements]
    (let [new-local-statements (concat local-statements more-statements)
          new-graph-data (update graph-data :entity-lookup #(tx/build-entity-lookup schema more-statements %))]
      (Graph. schema named-queries t new-local-statements new-graph-data)))


  (t [this] t)


  IHash
  (-hash [this]
    (hash (map hash [schema named-queries t local-statements])))


  IEquiv
  (-equiv [this other]
    (= (hash this) (hash other)))


  GraphPrivate
  (set-state! [this pulled-trees-map t']
    (set! graph-data (->graph-data schema pulled-trees-map local-statements))
    (set! t t'))

  GraphSSR
  (named-queries* [this] named-queries)
  (pulled-trees-map* [this] (:pulled-trees-map graph-data)))
