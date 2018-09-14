(ns hyperfiddle.security
  (:require
    [cats.core :refer [mlet]]
    [cats.monad.either :as either :refer [right]]
    [contrib.eval :refer [safe-eval-string+]]
    [contrib.try$ :refer [try-either]]
    [taoensso.timbre :as timbre]))


(def root "hyperfiddle.security/root")                      ; todo uuid/real account

(defn tx-validation-failure [& {:as data-map}]
  (ex-info "user tx failed validation" (into {:hyperfiddle.io/http-status-code 403} data-map)))

(defn write-allow-anonymous [hf-db subject tx]
  tx)

(defn write-authenticated-users-only [hf-db subject tx]
  (if (nil? subject)
    (throw (tx-validation-failure))
    tx))

(defn write-owner-only [hf-db subject tx]
  (if (-> (into #{root} (:hyperfiddle/owners hf-db))
          (contains? subject))
    tx
    (throw (tx-validation-failure))))

(let [memoized-safe-eval-string (memoize safe-eval-string+)]
  (defn attempt-to-transact? [hf-db subject]
    (case (get-in hf-db [:database/write-security :db/ident])
      ::owner-only (-> (into #{} (:hyperfiddle/owners hf-db))
                       (contains? subject)
                       (right))
      ::authenticated-users-only (right (boolean subject))
      ::allow-anonymous (right true)
      ::custom (mlet [sec (memoized-safe-eval-string (:database.custom-security/client hf-db))
                      :let [f (or (::subject-can-transact? sec) (constantly true))]]
                 (try-either (f hf-db subject)))
      (right true))))

(let [memoized-safe-eval-string (memoize safe-eval-string+)]
  (defn writable-entity? [hf-db subject m]
    (case (get-in hf-db [:database/write-security :db/ident])
      ::owner-only (-> (into #{} (:hyperfiddle/owners hf-db))
                       (contains? subject))
      ::authenticated-users-only (boolean subject)
      ::allow-anonymous true
      ::custom (-> (mlet [sec (memoized-safe-eval-string (:database.custom-security/client hf-db))
                          :let [f (or (::writable-entity? sec) (constantly true))]]
                     (try-either (f hf-db subject m)))
                   (either/branch
                     (fn [e]
                       (timbre/error e)
                       false)
                     identity))
      true)))
