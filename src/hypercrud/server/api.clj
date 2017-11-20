(ns hypercrud.server.api
  (:require [clojure.set :as set]
            [clojure.walk :as walk]
            [datascript.parser :as parser]
            [datomic.api :as d]
            [hypercrud.types.DbVal]
            [hypercrud.types.Entity :refer [->Entity]]
            [hypercrud.types.EntityRequest]
            [hypercrud.types.Err :refer [->Err]]
            [hypercrud.types.QueryRequest]
            [hypercrud.util.branch :as branch]
            [hypercrud.util.core :as util]
            [hypercrud.util.identity :as identity])
  (:import (hypercrud.types.DbVal DbVal)
           (hypercrud.types.EntityRequest EntityRequest)
           (hypercrud.types.QueryRequest QueryRequest)))


(defmulti parameter (fn [this & args] (class this)))

(defmethod parameter :default [this & args] this)

(defmethod parameter DbVal [dbval get-secure-db-with]
  (-> (get-secure-db-with (:uri dbval) (:branch dbval)) :db))

(defn recursively-add-entity-types [pulled-tree dbval]
  (walk/postwalk (fn [o]
                   (if (:db/id o)
                     (->Entity dbval o)
                     o))
                 pulled-tree))

(defmulti hydrate* (fn [this & args] (class this)))

(defmethod hydrate* EntityRequest [{:keys [e a db pull-exp]} get-secure-db-with]
  (let [{pull-db :db} (get-secure-db-with (:uri db) (:branch db))
        pull-exp (if a [{a pull-exp}] pull-exp)
        pulled-tree (if (identity/tempid? e)
                      (if a
                        nil
                        ; todo return a positive id here
                        {:db/id e})
                      (d/pull pull-db pull-exp e))
        pulled-tree (recursively-add-entity-types pulled-tree db)
        pulled-tree (if a (get pulled-tree a) pulled-tree)]
    pulled-tree))

(defn process-result [user-params fe result]
  (condp = (type fe)
    datascript.parser.Variable result
    datascript.parser.Pull (let [dbval (->> (get-in fe [:source :symbol])
                                            str
                                            (get user-params))]
                             (recursively-add-entity-types result dbval))
    datascript.parser.Aggregate result))

(defn process-scalar [user-params qfind result]
  (process-result user-params (:element qfind) result))

(defn process-tuple [user-params qfind result]
  (mapv (partial process-result user-params)
        (:elements qfind)
        result))

(defmethod hydrate* QueryRequest [{:keys [query params]} get-secure-db-with]
  (assert query "hydrate: missing query")
  (let [{:keys [qfind]} (parser/parse-query query)
        ordered-params (->> (util/parse-query-element query :in)
                            (mapv #(get params (str %)))
                            (mapv #(parameter % get-secure-db-with)))
        ;todo gaping security hole
        result (apply d/q query ordered-params)]
    (condp = (type qfind)
      ; todo preserve set results
      datascript.parser.FindRel (mapv #(process-tuple params qfind %) result)
      datascript.parser.FindColl (mapv #(process-scalar params qfind %) result)
      datascript.parser.FindTuple (process-tuple params qfind result)
      datascript.parser.FindScalar (process-scalar params qfind result))))

(defn build-get-secure-db-with [staged-branches db-with-lookup]
  (letfn [(get-secure-db-from-branch [{:keys [branch-ident uri tx] :as branch}]
            (or (get @db-with-lookup branch)
                (let [{:keys [db id->tempid]} (if branch-ident
                                                (let [parent-ident (branch/decode-parent-branch branch-ident)
                                                      parent-branch (or (->> staged-branches
                                                                             (filter #(and (= parent-ident (:branch-ident %))
                                                                                           (= uri (:uri %))))
                                                                             first)
                                                                        {:branch-ident parent-ident
                                                                         :uri uri})]
                                                  (get-secure-db-from-branch parent-branch))
                                                {:db (d/db (d/connect (str uri)))})
                      ; is it a history query? (let [db (if (:history? dbval) (d/history db) db)])
                      _ (let [validate-tx (constantly true)]
                          ; todo look up tx validator
                          (assert (validate-tx db tx) (str "staged tx for " uri " failed validation")))
                      project-db-with (let [read-sec-predicate (constantly true) ;todo lookup sec pred
                                            ; todo d/with an unfiltered db
                                            {:keys [db-after tempids]} (d/with db tx)]
                                        {:db (d/filter db-after read-sec-predicate)
                                         ; todo this merge is excessively duplicating data to send to the client
                                         :id->tempid (merge id->tempid (set/map-invert tempids))})]
                  (swap! db-with-lookup assoc branch project-db-with)
                  project-db-with)))]
    (fn [uri branch-val]
      (let [branch (or (->> staged-branches
                            (filter #(and (= branch-val (:branch-val %))
                                          (= uri (:uri %))))
                            first)
                       {:branch-val branch-val
                        :uri uri})]
        (get-secure-db-from-branch branch)))))

(defn hydrate [staged-branches request root-t]
  (let [db-with-lookup (atom {})
        get-secure-db-with (build-get-secure-db-with staged-branches db-with-lookup)
        pulled-trees-map (->> request
                              (mapv (juxt identity
                                          #(try (hydrate* % get-secure-db-with)
                                                (catch Throwable e
                                                  (.println *err* (pr-str e))
                                                  (->Err (str e))))))
                              (into {}))]
    {:t nil
     :pulled-trees-map pulled-trees-map
     :id->tempid (reduce (fn [acc [branch db]]
                           (assoc-in acc [(:uri branch) (:branch-val branch)] (:id->tempid db)))
                         {}
                         @db-with-lookup)}))

(defn transact! [dtx-groups]
  (let [valid? (every? (fn [[uri tx]]
                         (let [db (d/db (d/connect (str uri)))
                               ; todo look up tx validator
                               validate-tx (constantly true)]
                           (validate-tx db tx)))
                       dtx-groups)]
    (if-not valid?
      (throw (RuntimeException. "user tx failed validation"))
      (let [tempid-lookups (->> dtx-groups
                                (mapv (fn [[uri dtx]]
                                        (let [{:keys [tempids]} @(d/transact (d/connect (str uri)) dtx)]
                                          [uri tempids])))
                                (into {}))]
        {:tempid->id tempid-lookups}))))

(defn latest [conn]
  (str (-> (d/db conn) d/basis-t)))
