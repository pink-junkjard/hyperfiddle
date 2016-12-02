(ns hypercrud.client.graph
  (:require [cljs.pprint :as pprint]
            [goog.Uri]
            [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx]
            [hypercrud.types :refer [->DbId ->DbVal ->Entity]]
            [hypercrud.util :as util]))


(defprotocol DbGraphPrivate
  (set-db-graph-state! [this pulled-trees tempids t']))


(defprotocol SuperGraphPrivate
  (set-state! [this pulled-trees-map tempids t']))

;; Really we just want to be able to serialize graphs for the wire; this is a quick hack
;; because we aren't sure what we want to do about the schema which is part of the graph and the client
(defprotocol GraphSSR
  (named-queries* [this])
  (pulled-trees-map* [this]))


(defrecord GraphData [pulled-trees-map resultsets])


;; DbTouched
(deftype DbGraph [schema ^:mutable dbval local-statements ^:mutable data-lookup ^:mutable entity-lookup]
  hc/DbGraph
  (entity [this dbid]
    (assert (not= nil dbid) "Cannot request nil entity")
    (if-let [entity (get entity-lookup dbid)]
      entity
      (let [data (or (get data-lookup dbid)
                     ; graph should track tempids provided, and only return entities for where the id has been allocated
                     ; otherwise we should throw
                     (do
                       (assert (tx/tempid? dbid) (str "Entity not found locally: " dbid))
                       {:db/id dbid}))
            entity (->Entity this dbid data {})]
        (set! entity-lookup (assoc entity-lookup dbid entity))
        entity)))


  (with' [this more-statements]
    (let [new-local-statements (concat local-statements more-statements)
          new-data-lookup (tx/build-data-lookup schema more-statements data-lookup)]
      (DbGraph. schema dbval new-local-statements new-data-lookup {})))


  IHash
  (-hash [this]
    ; todo equality when t is nil
    (hash (map hash [schema dbval local-statements])))

  IEquiv
  (-equiv [this other]
    (= (hash this) (hash other)))

  DbGraphPrivate
  (set-db-graph-state! [this pulled-trees tempids t']
    (let [lookup (tx/pulled-tree-to-data-lookup schema (.-conn-id dbval) pulled-trees tempids)]
      (set! data-lookup (tx/build-data-lookup schema local-statements lookup))
      (set! entity-lookup {}))
    (set! dbval (->DbVal (.-conn-id dbval) t'))
    nil))


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
  (set-state! [this pulled-trees-map tempids t']
    ; pulled-trees-map :: Map[query List[List[Pulled-Tree]]]
    ;                               List[ResultHydrated]
    ; ResultHydrated :: Tuple[EntityHydrated]
    ; (vec pulled-trees-map) :: List[[query Pulled-Trees]]
    ; query :: [q params [dbval pull-exp]]

    ; result-sets :: Map[query -> List[List[DbId]]]
    (let [q->dbvals (fn [q pull-exps]
                      (->> (util/parse-query-element q :find)
                           (mapv str)
                           (mapv (fn [find-element]
                                   (let [[dbval _] (get pull-exps find-element)]
                                     dbval)))))]
      (let [resultset-by-query
            (->> pulled-trees-map
                 (mapv (fn [[query resultset-hydrated]]
                         (let [[q params pull-exps] query
                               ordered-dbvals (q->dbvals q pull-exps)
                               resultset-ids (mapv (fn [result-hydrated]
                                                     (mapv (fn [entity-hydrated dbval]
                                                             (let [id (:db/id entity-hydrated)
                                                                   id (or (get-in tempids [(.-conn-id dbval) id]) id)]
                                                               (->DbId id (.-conn-id dbval))))
                                                           result-hydrated ordered-dbvals))
                                                   resultset-hydrated)]
                           [query resultset-ids])))
                 (into {}))]
        (set! graph-data (GraphData. pulled-trees-map resultset-by-query)))

      ; grouped :: Map[DbVal -> PulledTrees]


      ;; group by dbval, and give each list of hydrated-entities to the proper DbGraph so it can do datom stuff

      (let [grouped (->> pulled-trees-map
                         (mapcat (fn [[[q params pull-exps] resultset-hydrated]]
                                   (let [transposed-resultset (util/transpose resultset-hydrated)]
                                     (->> (q->dbvals q pull-exps)
                                          (map vector transposed-resultset)))))
                         (group-by (fn [[pulled-trees dbval]] dbval))
                         (util/map-values (fn [things]      ;things :: List[[List[EntityHydrated] dbval]]
                                            ;; All of these pulled trees are from the same database value
                                            (mapcat (fn [[entities-hydrated dbval]] entities-hydrated) things))))]

        (doall (map (fn [[dbval graph]]
                      (let [new-ptm (get grouped dbval)
                            new-t nil]
                        (set-db-graph-state! graph new-ptm (get tempids (.-conn-id dbval)) new-t)))
                    graphs))))
    nil)

  GraphSSR
  (named-queries* [this] named-queries)
  (pulled-trees-map* [this]
    (assert false "deprecated. this does not have tempids replaced")
    (:pulled-trees-map graph-data)))


(defn superGraph [schemas named-queries]
  (let [graphs (->> schemas
                    (map (fn [[dbval schema]]
                           [dbval (->DbGraph schema dbval nil {} {})]))
                    (into {}))]
    (->SuperGraph named-queries graphs nil)))