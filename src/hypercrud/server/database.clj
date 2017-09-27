(ns hypercrud.server.database
  (:require [datomic.api :as d]
            [hypercrud.server.db-root :as db]
            [hypercrud.server.util.datomic-adapter :as datomic-adapter]
            [hypercrud.types.DbId :refer [->DbId]]))


(defn get-database-uri [root-db database-id]
  (let [db-name (-> (d/entity root-db database-id) d/touch :domain/ident)]
    ; todo https://tools.ietf.org/html/rfc3986#section-2
    (str db/transactor-uri db-name)))

(defn get-root-conn []
  (d/connect (str db/transactor-uri "root")))

(defn get-db-conn! [root-db database-id]
  (assert (or (number? database-id) (coll? database-id))
          (format "unsupported database-id `%s`; must be long or lookupref" database-id))
  (let [database-id (if-not (number? database-id)
                      (:db/id (d/entity root-db database-id))
                      database-id)
        database-uri (get-database-uri root-db database-id)]
    ; hydrate idents in database-id position
    (d/create-database database-uri)
    (d/connect database-uri)))

(defn resolve-hc-tempid [conn-id {:keys [db-after tempids] :as result} tempid]
  ;(assert (string? tempid) "hypercrud requires string tempids")
  (let [id (d/resolve-tempid db-after tempids tempid)]
    [(->DbId tempid conn-id) (->DbId (str id) conn-id)]))

(defn build-id->tempid-lookup [db-after tempids dtx]
  (->> dtx (mapv second) (into #{})
       (filter datomic-adapter/datomic-tempid?)
       (mapv (juxt #(d/resolve-tempid db-after tempids %)
                   datomic-adapter/did->hid))
       (into {})))
