(ns hyperfiddle.io.sync
  #?(:clj
     (:refer-clojure :exclude [sync]))
  (:require #?(:clj [datomic.api :as d])
                    [hyperfiddle.io.http.core :refer [http-request!]]
                    [promesa.core :as p]
                    [taoensso.timbre :as timbre])
  #?(:clj
     (:import (java.net URISyntaxException)
              (java.util.concurrent ExecutionException))))


#?(:clj
   (defn sync [dbs]                                         ; sync is the low level datomic call
     (timbre/debug "syncing" (pr-str dbs))
     ; ordered kv seq
     (try (->> dbs
               (mapcat (juxt identity #(-> % str d/connect d/sync deref d/basis-t)))
               (apply sorted-map))
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
            (throw e)))))

(defn sync-rpc! [service-uri dbs & [jwt]]
  (-> {:url (str service-uri "sync")
       :accept :application/transit+json :as :auto
       :method :post :form dbs
       :content-type :application/transit+json}
      (into (when jwt {:auth {:bearer jwt}}))
      (http-request!)
      (p/then (fn [{:keys [body]}]
                (->> body
                     (apply concat)
                     (apply sorted-map))))))
