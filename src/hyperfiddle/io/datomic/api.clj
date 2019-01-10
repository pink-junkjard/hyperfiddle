(ns hyperfiddle.io.datomic.api
  (:require
    [datomic.api :as d]
    [taoensso.timbre :as timbre])
  (:import
    (java.net URISyntaxException)
    (java.util.concurrent ExecutionException)))


(defn connect [uri]
  (try (d/connect uri)
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
