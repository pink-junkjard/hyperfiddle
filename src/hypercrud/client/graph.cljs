(ns hypercrud.client.graph
  (:require [cats.monad.exception :as exception]
            [goog.Uri]
            [hypercrud.client.core :as hc]
            [hypercrud.client.schema :as schema-util]
            [hypercrud.client.tx :as tx]
            [hypercrud.types :as types :refer [->DbId ->DbVal ->Entity ->DbError]]
            [hypercrud.util :as util]))


(defprotocol SuperGraphPrivate
  (set-state! [this editor-dbval editor-schema pulled-trees-map tempids]))


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


(deftype SuperGraph [request ^:mutable graphs ^:mutable graph-data]
  hc/SuperGraph
  (select [this query-request]
    (let [resultset-or-error (get (:resultsets graph-data) query-request)]
      (if (instance? types/DbError resultset-or-error)
        (exception/failure (js/Error. (.-msg resultset-or-error))) ;build a stack trace
        (exception/success resultset-or-error))))

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
      (SuperGraph. request new-graphs graph-data)))


  (t [this]
    (hash (map :dbval graphs)))

  IHash
  (-hash [this]
    (hash request))


  IEquiv
  (-equiv [this other]
    (= (hash this) (hash other)))


  SuperGraphPrivate
  (set-state! [this editor-dbval editor-schema pulled-trees-map tempids]
    ; pulled-trees-map :: Map[query List[List[Pulled-Tree]]]
    ;                               List[ResultHydrated]
    ; ResultHydrated :: Map[find-element EntityHydrated]
    ; (vec pulled-trees-map) :: List[[query Pulled-Trees]]
    ; query :: [q params [dbval pull-exp]]

    ; result-sets :: Map[query -> List[List[DbId]]]
    (let [resultset-by-query
          (->> pulled-trees-map
               (mapv (fn [[request hydrated-resultset-or-error]]
                       (let [resultset (if (instance? types/DbError hydrated-resultset-or-error)
                                         hydrated-resultset-or-error
                                         (condp = (type request)
                                           types/EntityRequest nil

                                           types/QueryRequest
                                           (mapv (fn [result-hydrated]
                                                   (->> result-hydrated
                                                        (mapv (fn [[find-element entity-hydrated]]
                                                                (let [find-element (str find-element)
                                                                      [dbval _] (get-in request [:pull-exps find-element])
                                                                      id (:db/id entity-hydrated)
                                                                      id (or (get-in tempids [(.-conn-id dbval) id]) id)]
                                                                  [find-element (->DbId id (.-conn-id dbval))])))
                                                        (into {})))
                                                 hydrated-resultset-or-error)))]
                         ;; resultset comes out of hc/select
                         [request resultset])))
               (into {}))]
      (set! graph-data (GraphData. pulled-trees-map resultset-by-query)))

    ; grouped :: Map[DbVal -> PulledTrees]


    ;; group by dbval, and give each list of hydrated-entities to the proper DbGraph so it can do datom stuff

    (let [grouped (->> pulled-trees-map
                       (mapv (fn [[request hydrated-resultset-or-error]]
                               (let [hydrated-resultset (if (instance? types/DbError hydrated-resultset-or-error)
                                                          [] ; no datoms to fill graph on errors
                                                          hydrated-resultset-or-error)]
                                 (condp = (type request)
                                   types/EntityRequest nil

                                   types/QueryRequest
                                   ; build a Map[dbval List[EntityHydrated]]
                                   (->> (:pull-exps request)
                                        (mapv (fn [[find-element [dbval _]]]
                                                {dbval (mapv (fn [result]
                                                               (get result (symbol find-element)))
                                                             hydrated-resultset)}))
                                        (apply merge-with concat))))))
                       (apply merge-with concat))
          editor-graph (let [ptm (get grouped editor-dbval)
                             tempids (get tempids (.-conn-id editor-dbval))]
                         (dbGraph editor-schema editor-dbval nil ptm tempids))
          graphs' (->> request
                       (filter #(instance? types/DbVal %))
                       (remove #(= % editor-dbval))
                       (mapv (juxt identity (fn [dbval]
                                              (let [attributes (->> (hc/select this dbval)
                                                                    (exception/extract)
                                                                    (mapv (fn [result]
                                                                            (let [attr-dbid (get result "?attr")]
                                                                              (hc/entity editor-graph attr-dbid)))))
                                                    schema (schema-util/build-schema attributes)
                                                    new-ptm (get grouped dbval)
                                                    tempids (get tempids (.-conn-id dbval))]
                                                (dbGraph schema dbval nil new-ptm tempids)))))
                       (into {editor-dbval editor-graph}))]
      (set! graphs graphs'))
    nil))


(defn ->super-graph [editor-schema request pulled-trees-map]
  (let [super-graph (->SuperGraph request {} nil)
        init-root-dbval (->DbVal hc/*root-conn-id* nil)
        tempids nil]
    (set-state! super-graph init-root-dbval editor-schema pulled-trees-map tempids)
    super-graph))
