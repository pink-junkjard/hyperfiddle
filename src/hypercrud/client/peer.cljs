(ns hypercrud.client.peer
  (:require [cats.monad.exception :as exception]
            [clojure.walk :as walk]
            [goog.Uri]
            [hypercrud.client.core :as hc]
            [hypercrud.client.schema :as schema-util]
            [hypercrud.types :as types :refer [->DbId ->DbVal ->DbError]]
            [hypercrud.util :as util]))


(defprotocol PeerPrivate
  (set-state! [this editor-dbval editor-schema pulled-trees-map tempids]))


(defrecord GraphData [pulled-trees-map resultsets])


(defn recursively-replace-ids [pulled-tree conn-id tempid-lookup]
  (let [replace-tempid (fn [id]
                         (let [id (or (get tempid-lookup id) id)]
                           (->DbId id conn-id)))]
    (walk/postwalk (fn [o]
                     (if (map? o)
                       (util/update-existing o :db/id replace-tempid)
                       o))
                   pulled-tree)))


(deftype Peer [requests ^:mutable dbval->schema ^:mutable data-cache local-statements]
  hc/Peer
  (hydrate [this request]
    (let [resultset-or-error (get (:resultsets data-cache) request)]
      (if (instance? types/DbError resultset-or-error)
        (exception/failure (js/Error. (.-msg resultset-or-error))) ;build a stack trace
        (exception/success resultset-or-error))))


  (t [this]
    (hash (keys dbval->schema)))


  IHash
  (-hash [this]
    (hash requests))


  IEquiv
  (-equiv [this other]
    (= (hash this) (hash other)))


  PeerPrivate
  (set-state! [this editor-dbval editor-schema pulled-trees-map tempids]
    ; pulled-trees-map :: Map[query List[List[Pulled-Tree]]]
    ;                               List[ResultHydrated]
    ; ResultHydrated :: Map[find-element EntityHydrated]
    ; (vec pulled-trees-map) :: List[[query Pulled-Trees]]
    ; query :: [q params [dbval pull-exp]]

    ; result-sets :: Map[query -> List[List[DbId]]]
    (let [resultset-by-request (->> pulled-trees-map
                                    ; todo this fix ids could happen on the server
                                    (mapv (fn [[request hydrated-resultset-or-error]]
                                            (let [resultset (if (instance? types/DbError hydrated-resultset-or-error)
                                                              hydrated-resultset-or-error
                                                              (condp = (type request)
                                                                types/EntityRequest
                                                                (recursively-replace-ids hydrated-resultset-or-error (get-in request [:dbval :conn-id]) (get tempids (get-in request [:dbval :conn-id])))

                                                                types/QueryRequest
                                                                (->> hydrated-resultset-or-error
                                                                     (mapv (fn [result]
                                                                             (->> result
                                                                                  (mapv (fn [[find-element pulled-tree]]
                                                                                          (let [find-element (str find-element)
                                                                                                [dbval pull-exp] (get-in request [:pull-exps find-element])
                                                                                                pulled-tree (recursively-replace-ids pulled-tree (:conn-id dbval) (get tempids (:conn-id dbval)))]
                                                                                            [find-element pulled-tree])))
                                                                                  (into {})))))))]
                                              [request resultset])))
                                    (into {}))
          schema-request-lookup (->> requests
                                     (filter #(instance? types/DbVal %))
                                     (mapv (juxt #(schema-util/schema-request editor-dbval %) identity))
                                     (into {}))
          resultset-by-request (merge resultset-by-request
                                      (->> resultset-by-request
                                           (util/map-keys #(get schema-request-lookup %))
                                           (remove (comp nil? first))
                                           (into {})))]
      (set! data-cache (GraphData. pulled-trees-map resultset-by-request)))

    (set! dbval->schema (->> requests
                             (filter #(instance? types/DbVal %))
                             (remove #(= % editor-dbval))
                             (mapv (juxt identity #(->> (hc/hydrate this %)
                                                        (exception/extract)
                                                        (mapv (fn [result] (get result "?attr")))
                                                        (schema-util/build-schema))))
                             (into {editor-dbval editor-schema})))
    nil))


(defn ->peer [editor-schema requests pulled-trees-map]
  (let [peer (->Peer requests {} nil nil)
        init-root-dbval (->DbVal hc/*root-conn-id* nil)
        tempids nil]
    (set-state! peer init-root-dbval editor-schema pulled-trees-map tempids)
    peer))
