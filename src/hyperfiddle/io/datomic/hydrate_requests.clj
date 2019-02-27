(ns hyperfiddle.io.datomic.hydrate-requests
  (:require
    [cats.core :as cats :refer [mlet]]
    [cats.monad.either :as either]
    [cats.monad.exception :as exception]
    [clojure.set :as set]
    [contrib.datomic]
    [datomic.api :as d]
    [hypercrud.types.DbVal]
    [hypercrud.types.EntityRequest]
    [hypercrud.types.Err :refer [->Err]]
    [hypercrud.types.QueryRequest]
    [hyperfiddle.branch :as branch]
    [hyperfiddle.domain :as domain]                         ; todo is this moral?
    [hyperfiddle.io.bindings]                               ; userland
    [taoensso.timbre :as timbre])
  (:import
    (hypercrud.types.DbRef DbRef)
    (hypercrud.types.EntityRequest EntityRequest)
    (hypercrud.types.QueryRequest QueryRequest)))


(defrecord SecureDbWith [db id->tempid])

(defmulti parameter (fn [this & args] (class this)))

(defmethod parameter :default [this & args] this)

(defmethod parameter DbRef [dbval get-secure-db-with]
  (-> (get-secure-db-with (:dbname dbval) (:branch dbval)) :db))

(defmulti hydrate-request* (fn [this & args] (class this)))

(defmethod hydrate-request* EntityRequest [{:keys [e db pull-exp]} get-secure-db-with]
  (let [{pull-db :db} (get-secure-db-with (:dbname db) (:branch db))]
    (cond
      (nil? e) nil                                          ; This is probably an error, report it? Datomic says: (d/pull $ [:db/id] nil) => #:db{:id nil}
      (contrib.datomic/tempid? e) {:db/id e}                ; This introduces sloppy thinking about time!   https://github.com/hyperfiddle/hyperfiddle/issues/584
      :happy (d/pull pull-db pull-exp e))))

(defmethod hydrate-request* QueryRequest [{:keys [query params]} get-secure-db-with]
  (assert query "hydrate: missing query")
  (->> (map #(parameter % get-secure-db-with) params)
       ;todo gaping security hole
       (apply d/q query)))

; todo i18n
(def ERROR-BRANCH-PAST ":hyperfiddle.error/basis-stale Branching the past is currently unsupported, please refresh your basis by refreshing the page")

(defn build-get-secure-db-with+ [domain staged-branches db-with-lookup local-basis]
  {:pre [(map? local-basis)
         (not-any? nil? (vals local-basis))]}
  (letfn [(filter-db [db]
            (let [read-sec-predicate (constantly true)]     ;todo look up sec pred
              (d/filter db read-sec-predicate)))
          (get-secure-db-from-branch+ [{:keys [branch-ident dbname tx] :as branch}]
            (or (get @db-with-lookup branch)
                (let [db-with+ (mlet [t (or (some-> (get local-basis dbname) exception/success)
                                            (exception/failure (ex-info (str "Basis not found") {:dbname dbname :local-basis local-basis})))
                                      init-db-with (if (branch/root-branch? branch-ident)
                                                     (exception/try-on
                                                       (let [uri (:database/uri (domain/database domain dbname))
                                                             $ (-> (d/connect (str uri)) d/db filter-db)]
                                                         {:db-with $
                                                          :secure-db (d/as-of $ t)}))
                                                     (let [parent-ident (branch/parent-branch-id branch-ident)
                                                           parent-branch (or (->> staged-branches
                                                                                  (filter #(and (= parent-ident (:branch-ident %))
                                                                                                (= dbname (:dbname %))))
                                                                                  first)
                                                                             {:branch-ident parent-ident
                                                                              :dbname dbname})]
                                                       (get-secure-db-from-branch+ parent-branch)))]
                                 ; is it a history query? (let [db (if (:history? dbval) (d/history db) db)])
                                 (if (empty? tx)
                                   (cats/return init-db-with)
                                   (mlet [:let [{:keys [db-with id->tempid with?]} init-db-with]
                                          _ (if (and (not with?) (not= t (d/basis-t db-with)))
                                              ; can only run this assert once, on the first time a user d/with's
                                              ; every subsequent d/with, the new db's basis will never again match the user submitted basis
                                              ; however this is fine, since the original t is already known good
                                              (exception/failure (RuntimeException. ERROR-BRANCH-PAST))
                                              (exception/success nil))
                                          _ (let [validate-tx (constantly true)] ; todo look up tx validator
                                              (if (validate-tx tx)
                                                (exception/success nil)
                                                (exception/failure (RuntimeException. (str "staged tx for " dbname " failed validation")))))
                                          ; todo d/with an unfiltered db
                                          {:keys [db-after tempids]} (exception/try-on (d/with db-with tx))
                                          db-with (exception/try-on (filter-db db-after))]
                                     ; as-of/basis-t gymnastics:
                                     ; https://gist.github.com/dustingetz/39f28f148942728c13edef1c7d8baebf/ee35a6af327feba443339176d371d9c7eaff4e51#file-datomic-d-with-interactions-with-d-as-of-clj-L35
                                     ; https://forum.datomic.com/t/interactions-of-d-basis-t-d-as-of-d-with/219
                                     (exception/try-on
                                       {:db-with db-with
                                        :secure-db (d/as-of db-with (d/basis-t db-with))
                                        :with? true
                                        ; todo this merge is excessively duplicating data to send to the client
                                        :id->tempid (merge id->tempid (set/map-invert tempids))}))))]
                  (swap! db-with-lookup assoc branch db-with+)
                  db-with+)))]
    (fn [dbname branch-ident]
      (let [branch (or (->> staged-branches
                            (filter #(and (= branch-ident (:branch-ident %))
                                          (= dbname (:dbname %))))
                            first)
                       {:branch-ident branch-ident
                        :dbname dbname})]
        (->> (get-secure-db-from-branch+ branch)
             (cats/fmap (fn [internal-secure-db]
                          (->SecureDbWith (:secure-db internal-secure-db) (:id->tempid internal-secure-db)))))))))

(defn extract-tempid-lookup+ [internal-secure-db+]
  (if (exception/success? internal-secure-db+)
    (either/right (:id->tempid @internal-secure-db+))
    (let [e (cats/extract internal-secure-db+)]
      (timbre/error e)
      (either/left (str e)))))

(defn extract-tempid-lookups [db-with-lookup branch-ident]
  ; oof these are crap data structures
  (->> @db-with-lookup
       (filter #(= branch-ident (:branch-ident (first %))))
       (reduce (fn [acc [branch internal-secure-db+]]
                 (assoc acc (:dbname branch) (extract-tempid-lookup+ internal-secure-db+)))
               {})))

(defn hydrate-request [get-secure-db-with+ request ?subject]
  {:pre [(or (instance? EntityRequest request) (instance? QueryRequest request))]}
  (binding [hyperfiddle.io.bindings/*subject* ?subject]
    (try (hydrate-request* request (comp exception/extract get-secure-db-with+))
         (catch Throwable e
           (timbre/error e)
           (->Err (str e))))))

(defn hydrate-requests [domain local-basis requests staged-branches ?subject]
  {:pre [requests
         (not-any? nil? requests)
         (every? #(or (instance? EntityRequest %) (instance? QueryRequest %)) requests)]}
  (let [db-with-lookup (atom {})
        local-basis (into {} local-basis)                   ; :: ([dbname 1234]), but there are some duck type shenanigans happening
        get-secure-db-with+ (build-get-secure-db-with+ domain staged-branches db-with-lookup local-basis)
        pulled-trees (->> requests
                          (map #(hydrate-request get-secure-db-with+ % ?subject))
                          (doall))
        tempid-lookups (reduce (fn [acc [branch internal-secure-db+]]
                                 (assoc-in acc [(:branch-ident branch) (:dbname branch)] (extract-tempid-lookup+ internal-secure-db+)))
                               {}
                               @db-with-lookup)]
    {:pulled-trees pulled-trees
     :tempid-lookups tempid-lookups}))
