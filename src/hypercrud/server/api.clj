(ns hypercrud.server.api
  (:require [clojure.set :as set]
            [clojure.walk :as walk]
            [datomic.api :as d]
            [hypercrud.server.util.datomic-adapter :as datomic-adapter]
            [hypercrud.types.DbId :refer [->DbId]]
            [hypercrud.types.DbVal :refer [->DbVal]]
            [hypercrud.types.DbError :refer [->DbError]]
            [hypercrud.types.EntityRequest]
            [hypercrud.types.QueryRequest]
            [hypercrud.util.core :as util])
  (:import (hypercrud.types.DbId DbId)
           (hypercrud.types.DbVal DbVal)
           (hypercrud.types.EntityRequest EntityRequest)
           (hypercrud.types.QueryRequest QueryRequest)))


(defn build-id->tempid-lookup [db-after tempids dtx]
  (->> dtx (mapv second) (into #{})
       (filter datomic-adapter/datomic-tempid?)
       (mapv (juxt #(d/resolve-tempid db-after tempids %)
                   datomic-adapter/did->hid))
       (into {})))

(defmulti parameter (fn [this & args] (class this)))

(defmethod parameter :default [this & args] this)

(defmethod parameter DbVal [dbval get-secure-db-with & args]
  (-> (get-secure-db-with (:uri dbval) (:branch dbval)) :db))

(defmethod parameter DbId [dbid get-secure-db-with & [dbval]]
  (let [uri (:uri (or dbval dbid))                  ; this will break when we remove connid from dbid
        db (get-secure-db-with uri (:branch dbval))
        hid (:id dbid)]                                     ; htempid (string) or did (num) or lookup ref
    (if (datomic-adapter/hc-tempid? hid)
      (get (:tempid->id db)
           hid                                              ; the with-db has seen this tempid in the staged-tx
           (Long/parseLong hid))                            ; dangling tempid not yet seen, it will pass through datomic. Due to datomic bug, it has to go through as a long
      hid)))                                                ; don't parse lookup refs or not-tempids

(defn recursively-replace-ids [pulled-tree uri id->tempid]
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
                           (->DbId hid uri)))]
    (walk/postwalk (fn [o]
                     (if (map? o)
                       (util/update-existing o :db/id replace-tempid)
                       o))
                   pulled-tree)))

(defmulti hydrate* (fn [this & args] (class this)))

(defmethod hydrate* EntityRequest [{:keys [e a dbval pull-exp]} get-secure-db-with]
  (try
    (let [{:keys [id->tempid] pull-db :db} (get-secure-db-with (:uri dbval) (:branch dbval))
          pull-exp (if a [{a pull-exp}] pull-exp)
          pulled-tree (d/pull pull-db pull-exp (parameter e get-secure-db-with dbval))
          pulled-tree (recursively-replace-ids pulled-tree (:uri dbval) id->tempid)
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
                   (let [{:keys [id->tempid] pull-db :db} (get-secure-db-with (:uri dbval) (:branch dbval))]
                     ; traverse tree, turning pulled :db/id into idents where possible?
                     (->> (d/pull-many pull-db pull-exp values)
                          (mapv #(recursively-replace-ids % (:uri dbval) id->tempid))))))
           (util/transpose)
           (mapv #(zipmap (mapv str ordered-find-element-symbols) %))))

    (catch Throwable e
      (.println *err* (pr-str e))
      (->DbError (str e)))))

(defn build-get-secure-db-with [hctx-groups]
  (let [db-with-lookup (atom {})]
    (fn get-secure-db-with [uri branch]
      (or (get-in @db-with-lookup [uri branch])
          (let [dtx (->> (get hctx-groups [uri branch])
                         (mapv datomic-adapter/stmt-dbid->id))
                db (d/db (d/connect (str uri)))
                ; is it a history query? (let [db (if (:history? dbval) (d/history db) db)])
                _ (let [validate-tx (constantly true)]
                    ; todo look up tx validator
                    (assert (validate-tx db dtx) (str "staged tx for " uri " failed validation")))
                project-db-with (let [read-sec-predicate (constantly true) ;todo lookup sec pred
                                      ; todo d/with an unfiltered db
                                      {:keys [db-after tempids]} (d/with db dtx)
                                      id->tempid (build-id->tempid-lookup db-after tempids dtx)]
                                  {:db (d/filter db-after read-sec-predicate)
                                   :id->tempid id->tempid
                                   :tempid->id (set/map-invert id->tempid)})]
            (swap! db-with-lookup assoc-in [uri branch] project-db-with)
            project-db-with)))))

(defn hydrate [hctx-groups request root-t]
  (let [get-secure-db-with (build-get-secure-db-with hctx-groups)
        pulled-trees-map (->> request
                              (mapv (juxt identity #(hydrate* % get-secure-db-with)))
                              (into {}))]
    {:t nil
     :pulled-trees-map pulled-trees-map}))

(defn transact! [htx]
  (let [dtx-groups (doall (util/map-values #(mapv datomic-adapter/stmt-dbid->id %) htx))
        valid? (every? (fn [[uri tx]]
                         (let [db (d/db (d/connect (str uri)))
                               ; todo look up tx validator
                               validate-tx (constantly true)]
                           (validate-tx db tx)))
                       dtx-groups)]
    (if-not valid?
      (throw (RuntimeException. "user tx failed validation"))
      (let [build-hc-tempid-lookup (fn [uri id->tempid]
                                     (->> id->tempid
                                          (mapv (fn [[id tempid]]
                                                  [(->DbId (str tempid) uri) (->DbId id uri)]))
                                          (into {})))
            hc-tempids (->> dtx-groups
                            (mapv (fn [[uri dtx]]
                                    (let [{:keys [db-after tempids]} @(d/transact (d/connect (str uri)) dtx)]
                                      (->> (build-id->tempid-lookup db-after tempids dtx)
                                           (build-hc-tempid-lookup uri)))))
                            (apply merge))]
        {:tempids hc-tempids}))))

(defn latest [conn]
  (str (-> (d/db conn) d/basis-t)))
