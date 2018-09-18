(ns hyperfiddle.security.client
  (:require
    [cats.core :refer [mlet return]]
    [cats.monad.maybe :as maybe]
    [cats.monad.either :as either :refer [right]]
    [contrib.ct :refer [maybe]]
    [contrib.eval :refer [safe-eval-string+]]
    [contrib.reactive :as r]
    [contrib.try$ :refer [try-either]]
    [hypercrud.browser.context :as context]
    [hypercrud.browser.field :as field]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.security :as security]
    [hyperfiddle.tempid :refer [tempid?]]
    [taoensso.timbre :as timbre]
    [hypercrud.util.branch :as branch]))


(def allow-anonymous
  {::subject-can-transact? (constantly true)
   ::writable-entity? (constantly true)})

(def authenticated-users-only
  {::subject-can-transact? (fn [hf-db subject] (some? subject))
   ::writable-entity? (fn [hf-db subject ctx] (some? subject))})

(def owner-only
  {::subject-can-transact? (fn [hf-db subject]
                             (-> (into #{} (:hyperfiddle/owners hf-db))
                                 (contains? subject)))
   ::writable-entity? (fn [hf-db subject ctx]
                        (-> (into #{} (:hyperfiddle/owners hf-db))
                            (contains? subject)))})

(let [parent-m (fn parent-m [ctx]
                 (when-let [ctx (:hypercrud.browser/parent ctx)]
                   (let [ident @(r/cursor (:hypercrud.browser/field ctx) [::field/path-segment])
                         isComponent @(context/hydrate-attribute ctx ident :db/isComponent)]
                     (if isComponent
                       (parent-m ctx)
                       @(:hypercrud.browser/data ctx)))))
      new-entity? (fn new-entity? [peer uri dbid branch]
                    (or (tempid? dbid)
                        (some? @(runtime/state peer [::runtime/partitions branch :tempid-lookups uri dbid]))
                        (if (some? branch)
                          (new-entity? peer uri dbid (branch/decode-parent-branch branch))
                          false)))]
  (def entity-ownership
    {::subject-can-transact? (fn [hf-db subject] (some? subject))
     ::writable-entity? (fn [hf-db subject ctx]
                          (and (some? subject)
                               (or (contains? (set (:hyperfiddle/owners hf-db)) subject)
                                   (-> (mlet [m (maybe (parent-m ctx))
                                              uri (maybe (context/uri ctx))]
                                         (return (or (new-entity? (:peer ctx) uri (:db/id m) (:branch ctx))
                                                     (contains? (set (:hyperfiddle/owners m)) subject))))
                                       ; ui probably in an invalid/error state when m or uri are nil
                                       (maybe/from-maybe false)))))}))

(let [memoized-safe-eval-string (memoize safe-eval-string+)]
  (defn- eval-client-sec [hf-db]
    (case (get-in hf-db [:database/write-security :db/ident] ::security/allow-anonymous) ; todo yank this default
      ::security/allow-anonymous (right allow-anonymous)
      ::security/authenticated-users-only (right authenticated-users-only)
      ::security/owner-only (right owner-only)
      ::security/custom (memoized-safe-eval-string (:database.custom-security/client hf-db)))))

(defn subject-can-transact? [hf-db subject]
  (mlet [client-sec (eval-client-sec hf-db)
         :let [f (or (::subject-can-transact? client-sec) (constantly true))]]
    (try-either (f hf-db subject))))

(defn writable-entity? [ctx]
  (-> (mlet [:let [dbname (context/dbname ctx)
                   hf-db (domain/dbname->hfdb dbname (:hypercrud.browser/domain ctx))
                   subject @(runtime/state (:peer ctx) [::runtime/user-id])]
             client-sec (eval-client-sec hf-db)
             :let [f (or (::writable-entity? client-sec) (constantly true))]]
        (try-either (f hf-db subject ctx)))
      (either/branch
        (fn [e]
          (timbre/error e)
          false)
        identity)))
