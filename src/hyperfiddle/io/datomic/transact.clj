(ns hyperfiddle.io.datomic.transact
  (:require
    [cats.monad.either :as either]
    [contrib.eval :as eval]
    [datomic.api :as d]
    [hyperfiddle.domain :as domain]                         ; todo immoral/circular
    [hyperfiddle.security :as security]
    [taoensso.timbre :as timbre]))


(let [memoized-eval-string!+ (memoize eval/eval-expr-str!+)]
  (defn process-tx [hf-db subject tx]
    (let [f (case (:database/write-security hf-db ::security/allow-anonymous) ; todo yank this default
              ::security/owner-only security/write-owner-only
              ::security/authenticated-users-only security/write-authenticated-users-only
              ::security/allow-anonymous security/write-allow-anonymous
              ::security/custom (-> (memoized-eval-string!+ (:database.custom-security/server hf-db))
                                    (either/branch
                                      (fn [e]
                                        (timbre/error e)
                                        (throw (ex-info "Misconfigured database security" {:hyperfiddle.io/http-status-code 500
                                                                                           :uri (:database/uri hf-db)
                                                                                           :additional-info (.getMessage e)})))
                                      :process-tx)))]
      (f hf-db subject tx))))


(defn transact! [domain subject tx-groups]
  {:pre [(not (uri? domain))]}                              ; this interface changed
  (let [tempid-lookups (->> tx-groups
                            (map (fn [[dbname tx]]
                                   [dbname (process-tx (domain/database domain dbname) subject tx)]))
                            (doall)                         ; allow any exceptions to fire before transacting anythng
                            (map (fn [[dbname dtx]]
                                   (let [uri (:database/uri (domain/database domain dbname))
                                         {:keys [tempids]} @(d/transact (d/connect (str uri)) dtx)]
                                     [dbname tempids])))
                            (into {}))]
    {:tempid->id tempid-lookups}))
