(ns hyperfiddle.io.datomic.hydrate-requests
  (:require [clojure.set :as set]
            [cuerdas.core :as str]
            [datomic.api :as d]
            [hypercrud.types.DbVal]
            [hypercrud.types.EntityRequest]
            [hypercrud.types.Err :refer [->Err]]
            [hypercrud.types.QueryRequest]
            [hypercrud.util.branch :as branch]
            [hyperfiddle.tempid :refer [tempid?]]
            [hyperfiddle.io.bindings]                       ; userland
            [taoensso.timbre :as timbre])
  (:import (hypercrud.types.DbVal DbVal)
           (hypercrud.types.EntityRequest EntityRequest)
           (hypercrud.types.QueryRequest QueryRequest)))


(defrecord SecureDbWith [db id->tempid])

(defmulti parameter (fn [this & args] (class this)))

(defmethod parameter :default [this & args] this)

(defmethod parameter DbVal [dbval get-secure-db-with]
  (-> (get-secure-db-with (:uri dbval) (:branch dbval)) :db))

(defmulti hydrate-request* (fn [this & args] (class this)))

(defmethod hydrate-request* EntityRequest [{:keys [e db pull-exp]} get-secure-db-with]
  (let [{pull-db :db} (get-secure-db-with (:uri db) (:branch db))]
    (if (tempid? e)
      {:db/id e}
      (d/pull pull-db pull-exp e))))

(defmethod hydrate-request* QueryRequest [{:keys [query params]} get-secure-db-with]
  (assert query "hydrate: missing query")
  (->> (map #(parameter % get-secure-db-with) params)
       ;todo gaping security hole
       (apply d/q query)))

; todo i18n
(def ERROR-BRANCH-PAST ":hyperfiddle.error/basis-stale Branching the past is currently unsupported, please refresh your basis by refreshing the page")

(defn build-get-secure-db-with [staged-branches db-with-lookup local-basis]
  {:pre [(not-any? nil? (vals local-basis))]}
  (letfn [(filter-db [db]
            (let [read-sec-predicate (constantly true)]     ;todo look up sec pred
              (d/filter db read-sec-predicate)))
          (get-secure-db-from-branch [{:keys [branch-ident uri tx] :as branch}]
            (assert (get local-basis uri) (str "busted local basis: " (pr-str uri) " not in " (pr-str local-basis)))
            (or (get @db-with-lookup branch)
                (let [t (get local-basis uri)
                      init-db-with (if branch-ident
                                     (let [parent-ident (branch/decode-parent-branch branch-ident)
                                           parent-branch (or (->> staged-branches
                                                                  (filter #(and (= parent-ident (:branch-ident %))
                                                                                (= uri (:uri %))))
                                                                  first)
                                                             {:branch-ident parent-ident
                                                              :uri uri})]
                                       (get-secure-db-from-branch parent-branch))
                                     (let [$ (-> (d/connect (str uri)) d/db filter-db)]
                                       {:db-with $
                                        :secure-db (d/as-of $ t)}))
                      ; is it a history query? (let [db (if (:history? dbval) (d/history db) db)])
                      db-with (if (empty? tx)
                                init-db-with
                                (let [{:keys [db-with id->tempid with?]} init-db-with
                                      _ (when (and (not with?) (not= t (d/basis-t db-with)))
                                          ; can only run this assert once, on the first time a user d/with's
                                          ; every subsequent d/with, the new db's basis will never again match the user submitted basis
                                          ; however this is fine, since the original t is already known good
                                          (throw (RuntimeException. ERROR-BRANCH-PAST)))
                                      _ (let [validate-tx (constantly true)] ; todo look up tx validator
                                          (assert (validate-tx tx) (str "staged tx for " uri " failed validation")))
                                      ; todo d/with an unfiltered db
                                      {:keys [db-after tempids]} (d/with db-with tx)
                                      db-with (filter-db db-after)]
                                  ; as-of/basis-t gymnastics:
                                  ; https://gist.github.com/dustingetz/39f28f148942728c13edef1c7d8baebf/ee35a6af327feba443339176d371d9c7eaff4e51#file-datomic-d-with-interactions-with-d-as-of-clj-L35
                                  ; https://forum.datomic.com/t/interactions-of-d-basis-t-d-as-of-d-with/219
                                  {:db-with db-with
                                   :secure-db (d/as-of db-with (d/basis-t db-with))
                                   :with? true
                                   ; todo this merge is excessively duplicating data to send to the client
                                   :id->tempid (merge id->tempid (set/map-invert tempids))}))]
                  (swap! db-with-lookup assoc branch db-with)
                  db-with)))]
    (fn [uri branch-ident]
      (let [branch (or (->> staged-branches
                            (filter #(and (= branch-ident (:branch-ident %))
                                          (= uri (:uri %))))
                            first)
                       {:branch-ident branch-ident
                        :uri uri})
            internal-secure-db (get-secure-db-from-branch branch)]
        (->SecureDbWith (:secure-db internal-secure-db) (:id->tempid internal-secure-db))))))

(defn hydrate-requests [local-basis requests staged-branches ?subject] ; theoretically, requests are grouped by basis for cache locality
  {:pre [requests
         (not-any? nil? requests)
         (every? #(or (instance? EntityRequest %) (instance? QueryRequest %)) requests)]}
  (timbre/debug (->> (map (comp #(str/prune % 400) pr-str) [local-basis staged-branches (count requests)]) (interpose ", ") (apply str "hydrate-requests: ")))
  (binding [hyperfiddle.io.bindings/*subject* ?subject]
    (let [db-with-lookup (atom {})
          get-secure-db-with (build-get-secure-db-with staged-branches db-with-lookup (into {} local-basis) #_":: ([uri 1234]), but there are some duck type shenanigans happening")
          pulled-trees (->> requests
                            (map #(try (hydrate-request* % get-secure-db-with)
                                       (catch Throwable e
                                         (timbre/error e)
                                         (->Err (str e)))))
                            (doall))
          ; this can also stream, as the request hydrates.
          tempid-lookups (reduce (fn [acc [branch internal-secure-db]]
                                   (assoc-in acc [(:branch-ident branch) (:uri branch)] (:id->tempid internal-secure-db)))
                                 {}
                                 @db-with-lookup)
          result {:pulled-trees pulled-trees
                  :tempid-lookups tempid-lookups}]
      result)))
