(ns hyperfiddle.io.datomic.peer
  (:require
    [contrib.datomic-peer]                                  ; query helpers
    [datomic.api :as d-peer]
    [hyperfiddle.io.datomic]
    [hyperfiddle.query]                                     ; query helpers
    [taoensso.timbre :as timbre])
  (:import
    (hyperfiddle.io.datomic DatomicFacade)
    (java.net URISyntaxException)
    (java.util.concurrent ExecutionException)))


(def impl
  (reify DatomicFacade
    (as-of [o db time-point] (d-peer/as-of db time-point))
    (basis [o conn] (-> conn d-peer/sync deref d-peer/basis-t))
    (basis-t [o db] (d-peer/basis-t db))
    (connect [o hf-db]
      (try (-> hf-db :database/uri str d-peer/connect)
           (catch URISyntaxException e
             ; Illegal character in opaque part at index 30: datomic:free://datomic:4334/as df
             (throw (ex-info (.getMessage e) {:hyperfiddle.io/http-status-code 400})))
           (catch IllegalArgumentException e
             ; :db.error/invalid-db-uri Invalid database URI datomic:free://datomic:4334/?af/
             (throw (ex-info (.getMessage e) {:hyperfiddle.io/http-status-code 400})))
           (catch ExecutionException e
             ; bad host, bad port, or datomic is down
             ; todo validate scheme, host, and port before attempting to connect
             ; - Connection refused  java.net.PlainSocketImpl.socketConnect
             ; - UnknownHostException datomicasdf: unknown error  java.net.Inet6AddressImpl.lookupAllHostAddr (Inet6AddressImpl.java:-2)
             ; - Database is already closed (to disable automatic closing at VM shutdown, add \";DB_CLOSE_ON_EXIT=FALSE\" to the db URL) [90121-171]
             (timbre/error e)
             (throw (ex-info "Service Unavailable" {:hyperfiddle.io/http-status-code 503})))
           (catch RuntimeException e
             (cond
               (re-find #"Could not find [^ ]* in catalog" (.getMessage e))
               (throw (ex-info (.getMessage e) {:hyperfiddle.io/http-status-code 404}))
               :else (do
                       (timbre/error e)
                       (throw e))))
           (catch Exception e
             (timbre/error e)
             (throw e))))
    (db [o conn] (d-peer/db conn))
    (pull [o db arg-map] (d-peer/pull db (:selector arg-map) (:eid arg-map)))
    (q [o arg-map] (d-peer/query arg-map))
    (transact [o conn arg-map] @(d-peer/transact conn (:tx-data arg-map)))
    (with [o db arg-map] (d-peer/with db (:tx-data arg-map)))
    (with-db [o conn] (d-peer/db conn))))
