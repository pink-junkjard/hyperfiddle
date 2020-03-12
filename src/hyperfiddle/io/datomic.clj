(ns hyperfiddle.io.datomic
  (:require
    [hypercrud.types.DbRef])
  (:import
    (hypercrud.types.DbRef DbRef)
    (java.io FileNotFoundException)))


(defprotocol ConnectionFacade
  :extend-via-metadata true
  (basis [conn])
  (db [conn])
  (transact [conn arg-map])
  (with-db [conn]))

(defprotocol DbFacade
  :extend-via-metadata true
  (as-of [db time-point])
  (basis-t [db])
  (pull [db arg-map])
  (with [db arg-map]))

(def client-supported (delay (try (require 'datomic.client.api) true ; todo this is always true
                                  (catch FileNotFoundException e false))))

(def peer-supported (delay (try (require 'datomic.api) true
                                (catch FileNotFoundException e false))))

(defn dyna-client [arg-map]
  (require 'datomic.client.api)
  ((resolve 'datomic.client.api/client) arg-map))

(defn dyna-connect [{:keys [database/uri database/db-name] :as hf-db} ?client]
  (cond
    (and uri @peer-supported) (do (require 'hyperfiddle.io.datomic.peer)
                                  ((resolve 'hyperfiddle.io.datomic.peer/connect) uri))
    (and db-name @client-supported ?client) (do (require 'hyperfiddle.io.datomic.client)
                                                ((resolve 'hyperfiddle.io.datomic.client/connect) ?client db-name))
    (and (nil? uri) (nil? db-name)) (throw (ex-info "Database not well formed, must specify a db-name or uri to connect to" {}))
    (and uri (not @peer-supported)) (throw (ex-info "Unable to resolve datomic peer library on classpath" {:database/uri uri}))
    (and db-name (nil? ?client)) (throw (ex-info "No datomic client provided, cannot connect" {:database/db-name db-name}))
    (and db-name (not @client-supported)) (throw (ex-info "Unable to resolve datomic client library on classpath" {:database/db-name db-name}))
    ))

(defn qf [dbs params]
  (let [{:keys [dbname]} (some #(when (instance? DbRef %) %) params)
        {:keys [database/uri database/db-name]} (get dbs dbname)]
    (cond
      (and uri @peer-supported) (do (require 'hyperfiddle.io.datomic.peer)
                                    (resolve 'hyperfiddle.io.datomic.peer/q))
      (and db-name @client-supported) (do (require 'hyperfiddle.io.datomic.client)
                                          (resolve 'hyperfiddle.io.datomic.client/q))
      (and (nil? uri) (nil? db-name)) (throw (ex-info "Database not well formed, must specify a db-name or uri to connect to" {:dbname dbname}))
      (and uri (not @peer-supported)) (throw (ex-info "Unable to resolve datomic peer library on classpath" {:database/uri uri}))
      (and db-name (not @client-supported)) (throw (ex-info "Unable to resolve datomic client library on classpath" {:database/db-name db-name}))
      )))
