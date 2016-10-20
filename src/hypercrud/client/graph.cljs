(ns hypercrud.client.graph
  (:require [cljs.pprint :as pprint]
            [goog.Uri]
            [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx]
            [hypercrud.util :as util]
            [hypercrud.types :as types]))


(defprotocol DbGraphPrivate
  (set-db-graph-state! [this pulled-trees-map t'])
  (schema [this]))


(defprotocol SuperGraphPrivate
  (set-state! [this pulled-trees-map' t'])
  (graph-data [this]))

;; Really we just want to be able to serialize graphs for the wire; this is a quick hack
;; because we aren't sure what we want to do about the schema which is part of the graph and the client
(defprotocol GraphSSR
  (named-queries* [this])
  (pulled-trees-map* [this]))


(defn ->pulled-trees [pulled-trees-map]
  (apply concat (vals pulled-trees-map)))


(defn ->resultsets [pulled-trees-map]
  (util/map-values #(map :db/id %) pulled-trees-map))


(defrecord GraphData [pulled-trees-map resultsets])


;; DbTouched
(deftype DbGraph [schema ^:mutable dbval local-statements ^:mutable entity-lookup]
  hc/DbGraph
  (entity' [this dbid]
    (assert (not= nil dbid) "Cannot request nil entity")
    ; graph should track tempids provided, and only return entities for where the id has been allocated
    ; otherwise we should throw
    (or
      (get entity-lookup dbid)
      (do
        (assert (tx/tempid? dbid) (str "Entity not found locally: " dbid))
        (with-meta {:db/id dbid}
                   {:dbval dbval}))))


  (with' [this more-statements]
    (let [new-local-statements (concat local-statements more-statements)
          new-graph-data (tx/build-entity-lookup schema more-statements dbval entity-lookup)]
      (DbGraph. schema dbval new-local-statements new-graph-data)))


  IHash
  (-hash [this]
    (hash (map hash [schema dbval local-statements])))

  IEquiv
  (-equiv [this other]
    (= (hash this) (hash other)))

  DbGraphPrivate
  (set-db-graph-state! [this pulled-trees-map t']
    (let [lookup (->> (->pulled-trees pulled-trees-map)
                      (map #(tx/pulled-tree-to-entities schema %))
                      (apply merge-with merge))]
      (set! entity-lookup (tx/build-entity-lookup schema local-statements dbval lookup)))
    (set! dbval (types/->DbVal (:conn-id dbval) t')))
  (schema [this] schema))


(deftype SuperGraph [named-queries ^:mutable graphs ^:mutable graph-data]
  hc/SuperGraph
  (select [this named-query] (hc/select this named-query nil))

  (select [this named-query query-message]
    (assert (contains? named-queries named-query)
            (let [named-query (or query-message named-query)]
              (str "Named-query: " named-query " not found in:\n" (with-out-str (pprint/pprint named-queries)))))
    (get (:resultsets graph-data) (get named-queries named-query)))


  (entity [this dbval dbid]
    (assert (not= nil dbval) "dbval cannot be nil")
    (assert (not= nil dbid) "Cannot request nil entity")
    (hc/entity' (get graphs dbval) dbid))


  (with [this dbval more-statements]
    (assert (not= nil dbval) "dbval cannot be nil")
    (SuperGraph. named-queries
                 (update graphs dbval #(hc/with' % more-statements))
                 graph-data))


  (t [this]
    (hash (map :dbval graphs)))

  IHash
  (-hash [this]
    (hash (map hash [(vals graphs) named-queries])))


  IEquiv
  (-equiv [this other]
    (= (hash this) (hash other)))


  SuperGraphPrivate
  (set-state! [this pulled-trees-map t']
    ;todo parse this map
    (set! graph-data (GraphData. pulled-trees-map (->resultsets pulled-trees-map)))
    (set! graphs (util/map-values (fn [graph]
                                    ;todo
                                    graph) graphs)))

  (graph-data [this] graph-data)


  GraphSSR
  (named-queries* [this] named-queries)
  (pulled-trees-map* [this] (:pulled-trees-map graph-data)))


(defn superGraph [schemas named-queries]
  (let [graphs (->> schemas
                    (map (fn [[dbval schema]]
                           [dbval (->DbGraph schema dbval nil nil)]))
                    (into {}))]
    (->SuperGraph named-queries graphs nil)))