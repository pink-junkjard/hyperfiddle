(ns hyperfiddle.database.fixtures
  (:require
    [clojure.test :refer [join-fixtures]]
    [datomic.api :as d]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.io.datomic.peer :as peer]                  ; todo run tests for client as well
    [hyperfiddle.io.datomic.transact :as transact]))


(defn init-domain [domain & {:keys [subject schemas init-txs]}]
  (->> (domain/databases domain)
       (map (fn [[dbname {:keys [:database/uri]}]]
              (fn [f]
                (when-not (d/create-database (str uri))
                  (throw (ex-info "Database already exists" {:dbname dbname :uri uri})))
                (try
                  (when-let [schema (get schemas dbname)]
                    (transact/transact! peer/impl domain subject {dbname schema}))
                  (when-let [init-tx (get init-txs dbname)]
                    (transact/transact! peer/impl domain subject {dbname init-tx}))
                  (f)
                  (finally
                    (when-not (d/delete-database (str uri))
                      (throw (ex-info "Database already deleted" {:dbname dbname :uri uri}))))))))
       (join-fixtures)))
