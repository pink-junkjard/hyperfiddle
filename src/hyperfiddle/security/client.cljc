(ns hyperfiddle.security.client
  (:require
    [cats.core :refer [mlet return]]
    [cats.monad.maybe :as maybe]
    [cats.monad.either :as either :refer [right]]
    [contrib.ct :refer [maybe]]
    [contrib.datomic]
    [contrib.eval :refer [eval-expr-str!+]]
    [contrib.reactive :as r]
    [contrib.try$ :refer [try-either]]
    [hypercrud.browser.context :as context]
    [hyperfiddle.branch :as branch]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.security :as security]
    [taoensso.timbre :as timbre]))


(def allow-anonymous
  {:subject-can-transact? (constantly true)
   :can-create? (constantly true)
   :writable-entity? (constantly true)})

(def authenticated-users-only
  {:subject-can-transact? (fn [hf-db subject] (some? subject))
   :can-create? (fn [hf-db subject ctx] (some? subject))
   :writable-entity? (fn [hf-db subject ctx] (some? subject))})

(let [owned-by? (fn [hf-db subject]
                  (-> (into #{} (:hyperfiddle/owners hf-db))
                      (contains? subject)))]
  (def owner-only
    {:subject-can-transact? (fn [hf-db subject] (owned-by? hf-db subject))
     :can-create? (fn [hf-db subject ctx] (owned-by? hf-db subject))
     :writable-entity? (fn [hf-db subject ctx] (owned-by? hf-db subject))}))

(let [parent-m (fn parent-m [ctx]
                 (if (:db/isComponent (context/attr ctx (context/a ctx)))
                   (parent-m (:hypercrud.browser/parent ctx))
                   (hypercrud.browser.context/data ctx)))
      new-entity? (fn new-entity? [peer dbname dbid branch]
                    (or (contrib.datomic/tempid? dbid)
                        (some-> @(runtime/state peer [::runtime/partitions branch :tempid-lookups dbname])
                                deref
                                (get dbid)
                                some?)
                        (and (not (branch/root-branch? branch))
                             (new-entity? peer dbname dbid (branch/parent-branch-id branch)))))]
  (def entity-ownership
    {:subject-can-transact? (fn [hf-db subject] (some? subject))
     :can-create? (fn [hf-db subject ctx] (some? subject))
     :writable-entity? (fn [hf-db subject ctx]
                         (and (some? subject)
                              (or (contains? (set (:hyperfiddle/owners hf-db)) subject)
                                  (-> (mlet [m (maybe (parent-m ctx))
                                             dbname (maybe (context/dbname ctx))]
                                        (return (or (new-entity? (:peer ctx) dbname (:db/id m) (:branch ctx))
                                                    (contains? (set (:hyperfiddle/owners m)) subject))))
                                      ; ui probably in an invalid/error state when m or uri are nil
                                      (maybe/from-maybe false)))))}))

(let [memoized-safe-eval-string (memoize eval-expr-str!+)]
  (defn- eval-client-sec [hf-db]
    (case (get-in hf-db [:database/write-security :db/ident] ::security/allow-anonymous) ; todo yank this default
      ::security/allow-anonymous (right allow-anonymous)
      ::security/authenticated-users-only (right authenticated-users-only)
      ::security/owner-only (right owner-only)
      ::security/custom (memoized-safe-eval-string (:database.custom-security/client hf-db)))))

(defn subject-can-transact? [hf-db subject]
  (mlet [client-sec (eval-client-sec hf-db)
         :let [f (or (:subject-can-transact? client-sec) (constantly true))]]
    (try-either (f hf-db subject))))

(defn can-create? [ctx]
  (-> (mlet [:let [dbname (context/dbname ctx)
                   hf-db (-> (runtime/domain (:peer ctx))
                             (domain/database dbname))
                   subject @(runtime/state (:peer ctx) [::runtime/user-id])]
             client-sec (eval-client-sec hf-db)
             :let [f (or (:can-create? client-sec) (constantly true))]]
        (try-either (f hf-db subject ctx)))
      (either/branch
        (fn [e]
          (timbre/error e)
          false)
        identity)))

(defn writable-entity? [ctx]
  (-> (mlet [:let [dbname (context/dbname ctx)
                   hf-db (-> (runtime/domain (:peer ctx))
                             (domain/database dbname))
                   subject @(runtime/state (:peer ctx) [::runtime/user-id])]
             client-sec (eval-client-sec hf-db)
             :let [f (or (:writable-entity? client-sec) (constantly true))]]
        (try-either (f hf-db subject ctx)))
      (either/branch
        (fn [e]
          (timbre/error e)
          false)
        identity)))
