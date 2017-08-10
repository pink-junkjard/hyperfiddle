(ns hypercrud.server.api
  (:require [clojure.walk :as walk]
            [datomic.api :as d]
            [hypercrud.server.database :as database]
            [hypercrud.server.db-root :as db]
            [hypercrud.server.util.datomic-adapter :as datomic-adapter]
            [hypercrud.types.DbId :refer [->DbId]]
            [hypercrud.types.DbVal :refer [->DbVal]]
            [hypercrud.types.DbError :refer [->DbError]]
            [hypercrud.types.EntityRequest]
            [hypercrud.types.QueryRequest]
            [hypercrud.util.branch :as branch]
            [hypercrud.util.core :as util])
  (:import (hypercrud.types.DbId DbId)
           (hypercrud.types.DbVal DbVal)
           (hypercrud.types.EntityRequest EntityRequest)
           (hypercrud.types.QueryRequest QueryRequest)))


(defn get-secure-db [dbval root-db]
  (if (= db/root-id (.conn-id dbval))
    root-db
    (let [security-predicate (constantly true)              ;todo lookup
          ; database-id could be a lookup ref in hypercrud fixtures
          conn (database/get-db-conn! root-db (.conn-id dbval))
          db (d/db conn)
          t (or (.branch dbval) (d/basis-t db))]
      (d/filter (d/as-of db (d/t->tx t)) security-predicate))))

(defmulti parameter (fn [this & args] (class this)))

(defmethod parameter :default [this & args] this)

(defmethod parameter DbVal [dbval get-secure-db-with & args]
  (-> (get-secure-db-with (:conn-id dbval) (:branch dbval)) :db))

(defmethod parameter DbId [dbid get-secure-db-with & [dbval]]
  (let [conn-id (:conn-id (or dbval dbid))                  ; this will break when we remove connid from dbid
        db (get-secure-db-with conn-id (:branch dbval))
        hid (:id dbid)]                                     ; htempid (string) or did (num) or lookup ref
    (if (datomic-adapter/hc-tempid? hid)
      (get (:tempid->id db)
           hid                                              ; the with-db has seen this tempid in the staged-tx
           (Long/parseLong hid))                            ; dangling tempid not yet seen, it will pass through datomic. Due to datomic bug, it has to go through as a long
      hid)))                                                ; don't parse lookup refs or not-tempids

(defn recursively-replace-ids [pulled-tree conn-id id->tempid]
  (let [replace-tempid (fn [did]

                         ; datomic tempid? can't know from the pulled-tree.
                         ; if we see it in the id->tempid we can fix it.
                         ; But what about tempids that aren't part of the tx? They should pass through.
                         ; -1 is still -1 in datomic.

                         ; either we saw it in the tx, fix it
                         ; or we have a negative id that we didn't see, fix it
                         ; or its legit

                         (let [hid (or (some-> (get id->tempid did) str)
                                       (if (> 0 did) (str did))
                                       did #_"not a tempid")]
                           (->DbId hid conn-id)))]
    (walk/postwalk (fn [o]
                     (if (map? o)
                       (util/update-existing o :db/id replace-tempid)
                       o))
                   pulled-tree)))

(defmulti hydrate* (fn [this & args] (class this)))

(defmethod hydrate* EntityRequest [{:keys [e a dbval pull-exp]} get-secure-db-with]
  (try
    (let [{:keys [id->tempid] pull-db :db} (get-secure-db-with (:conn-id dbval) (:branch dbval))
          pull-exp (if a [{a pull-exp}] pull-exp)
          pulled-tree (d/pull pull-db pull-exp (parameter e get-secure-db-with dbval))
          pulled-tree (recursively-replace-ids pulled-tree (:conn-id dbval) id->tempid)
          pulled-tree (if a (get pulled-tree a []) pulled-tree)]
      pulled-tree)
    (catch Throwable e
      (.println *err* (pr-str e))
      (->DbError (str e)))))

(defmethod hydrate* QueryRequest [{:keys [query params pull-exps]} get-secure-db-with]
  (try
    (assert query "hydrate: missing query")
    (let [ordered-params (->> (util/parse-query-element query :in)
                              (mapv #(get params (str %)))
                              (mapv #(parameter % get-secure-db-with)))
          ordered-find-element-symbols (util/parse-query-element query :find)
          ordered-pull-exps (->> ordered-find-element-symbols
                                 (mapv (fn [find-element-symbol]
                                         ; correlate
                                         (let [pull-exp (get pull-exps (str find-element-symbol))]
                                           (assert (not= nil pull-exp) (str "hydrate: missing pull expression for " find-element-symbol))
                                           pull-exp))))]
      (->> (apply d/q query ordered-params)                 ;todo gaping security hole
           (util/transpose)
           (util/zip ordered-pull-exps)
           (mapv (fn [[[dbval pull-exp] values]]
                   (let [{:keys [id->tempid] pull-db :db} (get-secure-db-with (:conn-id dbval) (:branch dbval))]
                     ; traverse tree, turning pulled :db/id into idents where possible?
                     (->> (d/pull-many pull-db pull-exp values)
                          (mapv #(recursively-replace-ids % (:conn-id dbval) id->tempid))))))
           (util/transpose)
           (mapv #(zipmap (mapv str ordered-find-element-symbols) %))))

    (catch Throwable e
      (.println *err* (pr-str e))
      (->DbError (str e)))))

(defn build-get-secure-db-with [db-with-lookup root-db hctx-groups root-validate-tx]
  (fn get-secure-db-with [conn-id branch]
    ; todo huge issues with lookup refs for conn-ids, they will have misses in the lookup cache and hctx-groups
    (or (get-in @db-with-lookup [conn-id branch])
        (let [dtx (->> (get-in hctx-groups [conn-id branch])
                       (mapv datomic-adapter/stmt-dbid->id))
              db (if branch
                   (:db (get-secure-db-with db-with-lookup root-db hctx-groups root-validate-tx conn-id (branch/decode-parent-branch branch)))
                   (let [db (d/db (database/get-conn! root-db conn-id))
                         project-migration-tx (database/migration-tx root-db db)]
                     (-> (d/with db project-migration-tx)
                         :db-after)))
              ; is it a history query? (let [db (if (:history? dbval) (d/history db) db)])
              ; todo look up relevant project tx validator
              _ (assert (root-validate-tx db dtx) (str "staged tx for " conn-id " failed validation"))
              ;todo lookup project sec pred
              read-sec-predicate (constantly true)
              ;; todo account for new attrs (a migration)
              project-db-with (database/with-tx (partial d/with db) read-sec-predicate dtx)]
          (swap! db-with-lookup assoc-in [conn-id branch] project-db-with)
          project-db-with))))

(defn hydrate [root-security-predicate root-validate-tx form root-t]
  (let [root-conn (database/get-root-conn)
        root-t (if root-t (-> (d/db root-conn) d/basis-t))
        {hctx-groups :staged-tx request :request} form

        ;todo move this filter hack to the client
        hctx-groups (->> hctx-groups
                         (map (fn [[conn-id branches]]
                                [conn-id
                                 (util/map-values
                                   (fn [branch]
                                     (filter (fn [[op e a v]]
                                               (not (and (or (= :db/add op) (= :db/retract op))
                                                         (nil? v))))
                                             branch))
                                   branches)]))
                         (into {}))

        root-dtx (->> (get-in hctx-groups [db/root-id nil])
                      (mapv datomic-adapter/stmt-dbid->id))
        root-db (d/db root-conn)                            ; run security here
        {root-db :db :as root-with} (database/with-tx (partial d/with root-db) root-security-predicate root-dtx)
        db-with-lookup (atom {db/root-id {nil root-with}})
        get-secure-db-with (build-get-secure-db-with db-with-lookup root-db hctx-groups root-validate-tx)
        pulled-trees-map (->> request
                              (mapv (juxt identity #(hydrate* % get-secure-db-with)))
                              (into {}))]
    {:t root-t
     :pulled-trees-map pulled-trees-map}))

(defn transact! [root-validate-tx htx]
  (let [root-conn (database/get-root-conn)
        root-db (d/db root-conn)                            ; tx validation needs schema, so gets unfiltered db
        dtx-groups (doall (->> htx
                               (util/map-keys (fn [conn-id]
                                                ;; todo the root transaction may contain the entity for conn-id
                                                ;; we need to first d/with the root transaction before we can can entity
                                                ; database-id could be a lookup ref in hypercrud fixtures
                                                (if-not (number? conn-id)
                                                  (:db/id (d/entity root-db conn-id))
                                                  conn-id)
                                                #_"resolve lookup refs in conn position"
                                                ))
                               (util/map-values #(mapv datomic-adapter/stmt-dbid->id %))))

        valid? (every? (fn [[conn-id tx]]
                         ;; conn-id allowed to be a tempid, resolve it (this requires already committing the root tx-group)
                         (let [maybe-db (if-not (datomic-adapter/hc-tempid? conn-id)
                                          (get-secure-db (->DbVal conn-id nil) root-db))]
                           ; todo look up relevant project tx validator, need to d/with the root transaction (validator may not be committed yet)
                           (root-validate-tx maybe-db tx)))
                       dtx-groups)]
    (if-not valid?
      (throw (RuntimeException. "user tx failed validation"))
      (let [hc-tx-uuid (d/squuid)                           ; suitable for cross database - https://groups.google.com/forum/?fromgroups=#!searchin/datomic/unique$20entity$20ids$20across$20databases/datomic/-ZESEw2ee1s/PDWIlwu9PGoJ

            ;; first transact the root - there may be new attributes which means project migrations
            root-result (database/hc-transact-one-color! root-db hc-tx-uuid db/root-id (get dtx-groups db/root-id))

            dtx-groups (->> dtx-groups
                            (util/map-keys (fn [conn-id]
                                             (if (and (not= db/root-id conn-id) (datomic-adapter/hc-tempid? conn-id))
                                               (get (:id->tempid root-result) conn-id)
                                               conn-id))))

            ;; for each project, there may be a migration
            migration-dtxs (->> (keys dtx-groups)
                                (mapv (fn [conn-id]
                                        (let [conn (database/get-conn! root-db conn-id)]
                                          (database/migration-tx (:db root-result) (d/db conn)))))
                                (zipmap (keys dtx-groups))
                                doall)

            ;; can migrations (attribute changes) can be in the same transaction as the user-tx?
            ;project-txs (merge-with concat migration-txs project-txs)

            ;; project-txs might reference a root tempid in the connection position of a dbid e.g.
            ;;    [:db/add #DbId[-100 -1] :post/title "first post"]
            ;; happens when our samples need fixtures

            hc-tempids (->> (concat (->> migration-dtxs
                                         (mapv (fn [[conn-id htx]]
                                                 (let [result (database/hc-transact-one-color! root-db hc-tx-uuid conn-id htx)]
                                                   (->> (:id->tempid result)
                                                        (mapv (fn [[id tempid]]
                                                                [(->DbId (str tempid) conn-id) (->DbId id conn-id)]))
                                                        (into {})))))
                                         doall)
                                    (->> (dissoc dtx-groups db/root-id)
                                         (mapv (fn [[conn-id htx]]
                                                 (let [result (database/hc-transact-one-color! root-db hc-tx-uuid conn-id htx)]
                                                   (->> (:id->tempid result)
                                                        (mapv (fn [[id tempid]]
                                                                [(->DbId (str tempid) conn-id) (->DbId id conn-id)]))
                                                        (into {})))))
                                         doall)
                                    [(->> (:id->tempid root-result)
                                          (mapv (fn [[id tempid]]
                                                  [(->DbId (str tempid) db/root-id) (->DbId id db/root-id)]))
                                          (into {}))])
                            (apply merge))]
        {:tempids hc-tempids}))))

(defn latest [conn]
  (str (-> (d/db conn) d/basis-t)))

(defn root-latest []
  (latest (database/get-root-conn)))
