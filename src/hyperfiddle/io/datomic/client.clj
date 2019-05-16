(ns hyperfiddle.io.datomic.client
  (:require
    [contrib.performance :as perf]
    [datomic.client.api :as d-client]
    [hyperfiddle.io.datomic]
    [taoensso.timbre :as timbre])
  (:import
    (hyperfiddle.io.datomic DatomicFacade)))


(defrecord ClientImpl [client]
  DatomicFacade
  (as-of [o db time-point] (d-client/as-of db time-point))
  (basis [o conn] (-> conn d-client/db :t))
  (basis-t [o db] (:t db))
  (connect [o hf-db]
    (try (let [db-name (:database/db-name hf-db)]
           (perf/time
             (fn [total-time]
               (timbre/debugf "Connecting to %s %sms" db-name total-time))
             (d-client/connect client {:db-name db-name})))
         (catch Exception e
           (timbre/error e)
           (throw e))))
  (db [o conn] (d-client/db conn))
  (pull [o db arg-map] (d-client/pull db arg-map))
  (q [o arg-map] (d-client/q arg-map))
  (transact [o conn arg-map] (d-client/transact conn arg-map))
  (with [o db arg-map] (d-client/with db arg-map))
  (with-db [o conn] (d-client/with-db conn)))

(defn build [arg-map] (->ClientImpl (d-client/client arg-map)))
