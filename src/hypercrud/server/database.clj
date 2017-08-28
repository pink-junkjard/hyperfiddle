(ns hypercrud.server.database
  (:require [clojure.set :as set]
            [datomic.api :as d]
            [hypercrud.server.context :as context]
            [hypercrud.server.db-root :as db]
            [hypercrud.server.util.datomic-adapter :as datomic-adapter]
            [hypercrud.types.DbId :refer [->DbId]]))


(defn get-database-uri [root-db database-id]
  (let [db-name (-> (d/entity root-db database-id) d/touch :database/ident)]
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

; should root-db just be in dynamic scope? threading it feels dumb but indicates that
; we may yet resolve a hypercrud lookup ref (in conn-id position)
(defn get-conn! [root-db conn-id]
  (if (= db/root-id conn-id)
    (get-root-conn)
    (get-db-conn! root-db conn-id)))

(defn resolve-hc-tempid [conn-id {:keys [db-after tempids] :as result} tempid]
  ;(assert (string? tempid) "hypercrud requires string tempids")
  (let [id (d/resolve-tempid db-after tempids tempid)]
    [(->DbId tempid conn-id) (->DbId (str id) conn-id)]))

(defn with-tx [f! read-sec-predicate dtx]
  ; todo hc-dtx needs to be yanked out
  (let [hc-dtx (if context/*user*
                 [{:db/id (d/tempid :db.part/tx)
                   :hypercrud/audit-user context/*user*
                   ;:hypercrud/tx hc-tx-uuid ;todo we need a temp squuid or something
                   }])
        {:keys [db-after tempids] :as result} (f! (concat dtx hc-dtx))
        db (-> db-after
               ;(d/as-of (d/db root-conn) (d/t->tx root-t))
               (d/filter read-sec-predicate))
        id->tempid (->> dtx (mapv second) (into #{}) (filter datomic-adapter/datomic-tempid?)
                        (mapv (juxt #(d/resolve-tempid db-after tempids %)
                                    datomic-adapter/did->hid))
                        (into {}))]
    {:db db
     :id->tempid id->tempid
     :tempid->id (set/map-invert id->tempid)}))

(defn hc-transact-one-color! [root-db hc-tx-uuid conn-id dtx]
  (let [conn (get-conn! root-db conn-id)]
    (with-tx (fn [& args] @(apply d/transact conn args)) (constantly true) dtx)))
