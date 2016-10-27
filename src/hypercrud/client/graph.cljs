(ns hypercrud.client.graph
  (:require [cljs.pprint :as pprint]
            [goog.Uri]
            [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx]
            [hypercrud.util :as util]
            [hypercrud.types :refer [->DbId ->Entity ->DbVal]]))


(defprotocol DbGraphPrivate
  (set-db-graph-state! [this pulled-trees t'])
  (schema [this]))


(defprotocol SuperGraphPrivate
  (set-state! [this pulled-trees-map' t']))

;; Really we just want to be able to serialize graphs for the wire; this is a quick hack
;; because we aren't sure what we want to do about the schema which is part of the graph and the client
(defprotocol GraphSSR
  (named-queries* [this])
  (pulled-trees-map* [this]))


(defrecord GraphData [pulled-trees-map resultsets])


;; DbTouched
(deftype DbGraph [schema ^:mutable dbval local-statements ^:mutable entity-lookup]
  hc/DbGraph
  (entity [this dbid]
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
    ; todo equality when t is nil
    (hash (map hash [schema dbval local-statements])))

  IEquiv
  (-equiv [this other]
    (= (hash this) (hash other)))

  DbGraphPrivate
  (set-db-graph-state! [this pulled-trees t']
    (let [lookup (->> pulled-trees
                      (map #(tx/pulled-tree-to-entities schema dbval %))
                      (apply merge-with merge))]
      (set! entity-lookup (tx/build-entity-lookup schema local-statements dbval lookup)))
    (set! dbval (->DbVal (.-conn-id dbval) t'))
    nil)
  (schema [this] schema))


(deftype SuperGraph [named-queries graphs ^:mutable graph-data]
  hc/SuperGraph
  (select [this named-query] (hc/select this named-query nil))

  (select [this named-query query-message]
    (assert (contains? named-queries named-query)
            (let [named-query (or query-message named-query)]
              (str "Named-query: " named-query " not found in:\n" (with-out-str (pprint/pprint named-queries)))))
    (get (:resultsets graph-data) (get named-queries named-query)))

  ; todo this goes away if dbval === dbgraph
  (get-dbgraph [this dbval]
    (assert (not= nil dbval) "dbval cannot be nil")
    (get graphs dbval))

  (with [this more-statements]
    ;currently not supporting time
    (let [grouped-stmts (group-by #(-> % second .-conn-id) more-statements)
          new-graphs (util/map-values (fn [graph]
                                        (let [conn-id (-> graph .-dbval .-conn-id)]
                                          (if-let [stmts (get grouped-stmts conn-id)]
                                            (hc/with' graph stmts)
                                            graph)))
                                      graphs)]
      (SuperGraph. named-queries new-graphs graph-data)))


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
    ; pulled-trees-map :: Map[query Pulled-Trees]
    ; (vec pulled-trees-map) :: List[[query Pulled-Trees]]
    ; query :: [q params [dbval pull-exp]]

    ; result-sets :: Map[query -> List[DbId]]
    (let [result-sets (->> pulled-trees-map
                           (map (fn [[query pulled-trees]]
                                  (let [[q params [dbval pull-exp]] query
                                        result-set (map (fn [pulled-tree]
                                                          (->DbId (:db/id pulled-tree) (.-conn-id dbval)))
                                                        pulled-trees)]
                                    [query result-set])))
                           (into {}))]
      (set! graph-data (GraphData. pulled-trees-map result-sets)))

    ; grouped :: Map[DbVal -> PulledTrees]
    (let [grouped (->> pulled-trees-map
                       ; Map[DbVal -> List[[query pulled-trees]]]
                       (group-by (fn [[[q params [dbval pull-exp]] pulled-trees]] dbval))
                       ; Map[DbVal -> PulledTrees]
                       (util/map-values (fn [pulled-trees-map]
                                          #_(map (fn [[[q params [dbval pull-exp]] pulled-trees]]) pulled-trees-map)

                                          ;; All of these pulled trees are from the same database value because of the
                                          ;; group-by, so can drop the key.
                                          (apply concat (vals pulled-trees-map)))))]

      (doall (map (fn [[dbval graph]]
                    (let [new-ptm (get grouped dbval)
                          new-t nil]
                      (set-db-graph-state! graph new-ptm new-t)))
                  graphs)))
    nil)

  GraphSSR
  (named-queries* [this] named-queries)
  (pulled-trees-map* [this] (:pulled-trees-map graph-data)))


(defn superGraph [schemas named-queries]
  (let [graphs (->> schemas
                    (map (fn [[dbval schema]]
                           [dbval (->DbGraph schema dbval nil nil)]))
                    (into {}))]
    (->SuperGraph named-queries graphs nil)))