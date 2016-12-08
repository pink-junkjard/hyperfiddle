(ns hypercrud.client.graph
  (:require [cljs.pprint :as pprint]
            [goog.Uri]
            [hypercrud.client.core :as hc]
            [hypercrud.client.schema :as schema-util]
            [hypercrud.client.tx :as tx]
            [hypercrud.types :as types :refer [->DbId ->DbVal ->Entity ->DbError]]
            [hypercrud.util :as util]))


(defprotocol SuperGraphPrivate
  (set-state! [this editor-dbval editor-schema pulled-trees-map tempids t']))

;; Really we just want to be able to serialize graphs for the wire; this is a quick hack
;; because we aren't sure what we want to do about the schema which is part of the graph and the client
(defprotocol GraphSSR
  (named-queries* [this])
  (pulled-trees-map* [this]))


(defrecord GraphData [pulled-trees-map resultsets])


;; DbTouched
(deftype DbGraph [schema dbval local-statements data-lookup ^:mutable entity-lookup]
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
    (hash (map hash [dbval local-statements])))

  IEquiv
  (-equiv [this other]
    (= (hash this) (hash other))))


(defn dbGraph [schema dbval local-statements pulled-trees tempids]
  (let [lookup (tx/pulled-tree-to-data-lookup schema (.-conn-id dbval) pulled-trees tempids)
        data-lookup (tx/build-data-lookup schema local-statements lookup)]
    (->DbGraph schema dbval local-statements data-lookup {})))


(deftype SuperGraph [named-queries ^:mutable graphs ^:mutable graph-data]
  hc/SuperGraph
  (select [this named-query] (hc/select this named-query nil))

  (select [this named-query query-message]
    #_(assert (contains? named-queries named-query)
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
    (hash named-queries))


  IEquiv
  (-equiv [this other]
    (= (hash this) (hash other)))


  SuperGraphPrivate
  (set-state! [this editor-dbval editor-schema pulled-trees-map tempids t']
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
                               resultset (if (instance? types/DbError resultset-hydrated)
                                           []               ; just ignore the error and show no results
                                           (mapv (fn [result-hydrated]
                                                   (mapv (fn [entity-hydrated dbval]
                                                           (let [id (:db/id entity-hydrated)
                                                                 id (or (get-in tempids [(.-conn-id dbval) id]) id)]
                                                             (->DbId id (.-conn-id dbval))))
                                                         result-hydrated ordered-dbvals))
                                                 resultset-hydrated))]
                           ;; resultset comes out of hc/select
                           [query resultset])))
                 (into {}))]
        (set! graph-data (GraphData. pulled-trees-map resultset-by-query)))

      ; grouped :: Map[DbVal -> PulledTrees]


      ;; group by dbval, and give each list of hydrated-entities to the proper DbGraph so it can do datom stuff

      (let [grouped (->> pulled-trees-map
                         (mapcat (fn [[[q params pull-exps] hydrated-resultset-or-error]]
                                   (let [hydrated-resultset-or-error (if (instance? types/DbError hydrated-resultset-or-error)
                                                                       [] ; ignore error, show no results
                                                                       hydrated-resultset-or-error)]
                                     (let [transposed-resultset (util/transpose hydrated-resultset-or-error)]
                                       (->> (q->dbvals q pull-exps)
                                            (map vector transposed-resultset))))))
                         (group-by (fn [[pulled-trees dbval]] dbval))
                         (util/map-values (fn [things]      ;things :: List[[List[EntityHydrated] dbval]]
                                            ;; All of these pulled trees are from the same database value
                                            (mapcat (fn [[entities-hydrated dbval]] entities-hydrated) things))))
            editor-graph (let [ptm (get grouped editor-dbval)
                               tempids (get tempids (.-conn-id editor-dbval))]
                           (dbGraph editor-schema editor-dbval nil ptm tempids))
            graphs' (->> (mapv first named-queries)
                         (filter #(instance? types/DbVal %))
                         (remove #(= % editor-dbval))
                         (mapv (juxt identity (fn [dbval]
                                                (let [attributes (->> (hc/select this dbval)
                                                                      (mapv (fn [[attr-dbid]] (hc/entity editor-graph attr-dbid))))
                                                      schema (schema-util/build-schema attributes)
                                                      new-ptm (get grouped dbval)
                                                      tempids (get tempids (.-conn-id dbval))]
                                                  (dbGraph schema dbval nil new-ptm tempids)))))
                         (into {editor-dbval editor-graph}))]
        (set! graphs graphs')))
    nil)

  GraphSSR
  (named-queries* [this] named-queries)
  (pulled-trees-map* [this]
    (assert false "deprecated. this does not have tempids replaced")
    (:pulled-trees-map graph-data)))
