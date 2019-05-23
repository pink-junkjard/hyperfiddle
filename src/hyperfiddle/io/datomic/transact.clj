(ns hyperfiddle.io.datomic.transact
  (:require
    [cats.monad.either :as either]
    [contrib.eval :as eval]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.io.datomic :as d]
    [hyperfiddle.security :as security]
    [taoensso.timbre :as timbre]))


(let [memoized-eval-string!+ (memoize eval/eval-expr-str!+)]
  (defn process-tx [hf-db subject tx]
    (let [f (case (get-in hf-db [:database/write-security :db/ident] ::security/allow-anonymous) ; todo yank this default
              ::security/owner-only security/write-owner-only
              ::security/authenticated-users-only security/write-authenticated-users-only
              ::security/allow-anonymous security/write-allow-anonymous
              ::security/custom (-> (memoized-eval-string!+ (:database.custom-security/server hf-db))
                                    (either/branch
                                      (fn [e]
                                        (timbre/debug (:database.custom-security/server hf-db))
                                        (timbre/error e)
                                        (throw (ex-info "Misconfigured database security" {:hyperfiddle.io/http-status-code 500
                                                                                           :uri (:database/uri hf-db)
                                                                                           :db-name (:database/db-name hf-db)
                                                                                           :additional-info (.getMessage e)})))
                                      :process-tx)))]
      (f hf-db subject tx))))


(defn transact! [domain subject tx-groups]
  (let [tempid-lookups (->> tx-groups
                            (map (fn [[dbname tx]]
                                   [dbname (process-tx (domain/database domain dbname) subject tx)]))
                            (doall)                         ; allow any exceptions to fire before transacting anythng
                            (map (fn [[dbname dtx]]
                                   (let [conn (domain/connect domain dbname)
                                         {:keys [tempids]} (d/transact conn {:tx-data dtx})]
                                     [dbname tempids])))
                            (into {}))]
    {:tempid->id tempid-lookups}))
