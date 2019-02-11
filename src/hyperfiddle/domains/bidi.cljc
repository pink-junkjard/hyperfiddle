(ns hyperfiddle.domains.bidi
  (:require
    [cats.core :refer [mlet return]]
    [cats.monad.either :as either]
    [contrib.reader :as reader]
    [contrib.try$ :refer [try-either]]
    [hypercrud.browser.router-bidi :as router-bidi]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.io.routes :as routes]
    [hyperfiddle.route :as route]))


(defrecord BidiDomain [ident fiddle-database databases environment router service-uri build]
  domain/Domain
  (ident [domain] ident)
  (fiddle-database [domain] fiddle-database)
  (databases [domain] databases)
  (environment [domain] environment)
  (url-decode [domain s]
    (either/branch
      (try-either (router-bidi/decode router s))
      (fn [e] (route/decoding-error e s))
      identity))
  (url-encode [domain route] (router-bidi/encode router route))
  (api-routes [domain] (routes/build build))
  (service-uri [domain] service-uri)
  )

(defn build+ [datomic-record service-uri build]
  (mlet [environment (reader/read-edn-string+ (:domain/environment datomic-record))
         :let [environment (assoc environment :domain/disable-javascript (:domain/disable-javascript datomic-record))]
         router (reader/read-edn-string+ (:domain/router datomic-record))]
    (return (map->BidiDomain
              {:ident (:domain/ident datomic-record)
               :fiddle-database (:domain/fiddle-database datomic-record)
               :databases (->> (:domain/databases datomic-record)
                               (map (juxt :domain.database/name :domain.database/record))
                               (into {}))
               :environment environment
               :router router
               :service-uri service-uri
               :build build}))))
