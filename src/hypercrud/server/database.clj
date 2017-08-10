(ns hypercrud.server.database
  (:require [clojure.set :as set]
            [datomic.api :as d]
            [hypercrud.server.context :as context]
            [hypercrud.server.db-root :as db]
            [hypercrud.server.util.datomic-adapter :as datomic-adapter]
            [hypercrud.types.DbId :refer [->DbId]]))


(declare reset-db!)

(defn database-uri [database-id]
  (str db/transactor-uri database-id))

(defn get-root-conn []
  (d/connect (str db/transactor-uri "root")))

(defn get-db-conn! [root-db database-id]
  (assert (or (number? database-id) (coll? database-id))
          (format "unsupported database-id `%s`; must be long or lookupref" database-id))
  (let [database-id (if-not (number? database-id)
                      (:db/id (d/entity root-db database-id))
                      database-id)]
    ; hydrate idents in database-id position
    (if (d/create-database (database-uri database-id))
      (reset-db! (constantly true) database-id))
    (d/connect (database-uri database-id))))

; should root-db just be in dynamic scope? threading it feels dumb but indicates that
; we may yet resolve a hypercrud lookup ref (in conn-id position)
(defn get-conn! [root-db conn-id]
  (if (= db/root-id conn-id)
    (get-root-conn)
    (get-db-conn! root-db conn-id)))

(defn hc-attr->datomic-schema-attr [attribute]
  (->> (select-keys attribute [:attribute/ident
                               :attribute/valueType
                               :attribute/cardinality
                               :attribute/id
                               :attribute/unique
                               :attribute/doc])
       (reduce (fn [acc [attr-key v]]
                 (if-not (nil? v) (assoc acc (keyword "db" (name attr-key)) v)))
               {:db/id (d/tempid :db.part/db)
                :db.install/_attribute :db.part/db})))

(defn resolve-hc-tempid [conn-id {:keys [db-after tempids] :as result} tempid]
  ;(assert (string? tempid) "hypercrud requires string tempids")
  (let [id (d/resolve-tempid db-after tempids tempid)]
    [(->DbId tempid conn-id) (->DbId (str id) conn-id)]))

(defn with-tx [f! read-sec-predicate dtx]
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

; This function isn't compatible with a staging area with migrating schema.
; We need to make this happen through hydrates, against a zero-db or something.
(defn reset-db! [root-sec database-id]
  (let [root-conn (get-root-conn)
        root-db (d/filter (d/db root-conn) root-sec)
        database-record (as-> '[:find ?db . :in $ ?db :where [?db]] $
                              (d/q $ root-db database-id)
                              (d/entity root-db $)
                              (d/touch $))                  ; npe if no db record; e.g. if tempid (the record is staged and we aren't accounting for root staging here)
        attributes (as-> '[:find [?attr ...] :where [?attr :attribute/ident]] $
                         (d/q $ root-db)
                         (map #(d/entity root-db %) $)
                         (map #(d/touch %) $))
        _ (assert database-record "no project found - failed security probably") ; would fail
        db-uri (database-uri database-id)
        _ (d/delete-database db-uri)]
    (try
      (let [_ (d/create-database db-uri)
            schema (->> (remove #(= :db/ident (:attribute/ident %)) attributes)
                        (mapv hc-attr->datomic-schema-attr))]
        @(d/transact (d/connect db-uri) schema))
      (catch Exception e
        (d/delete-database db-uri)
        (throw e)))))

(defn migration-tx [root-db project-db #_"could be an empty project database"]
  (let [attributes (as-> '[:find [?attr ...] :where [?attr :attribute/ident]] $
                         (d/q $ root-db)
                         (map #(d/entity root-db %) $)
                         (map #(d/touch %) $))
        installed-attribute-idents (set (d/q '[:find [?ident ...] :where [_ :db/ident ?ident]] project-db))]
    (->> attributes
         (remove #(contains? installed-attribute-idents (:attribute/ident %)))
         (mapv hc-attr->datomic-schema-attr))))
