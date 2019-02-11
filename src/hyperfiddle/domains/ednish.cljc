(ns hyperfiddle.domains.ednish
  (:require
    [cats.core :refer [mlet return]]
    [contrib.reader :as reader]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.io.routes :as routes]
    [hyperfiddle.route :as route]))


(defrecord EdnishDomain [ident fiddle-database databases environment home-route service-uri build]
  domain/Domain
  (ident [domain] ident)
  (fiddle-database [domain] fiddle-database)
  (databases [domain] databases)
  (environment [domain] environment)
  (url-decode [domain s] (route/url-decode s home-route))
  (url-encode [domain route] (route/url-encode route home-route))
  (api-routes [domain] (routes/build build))
  (service-uri [domain] service-uri)
  )

(defn build+ [datomic-record service-uri build]
  (mlet [environment (reader/read-edn-string+ (:domain/environment datomic-record))
         :let [environment (assoc environment :domain/disable-javascript (:domain/disable-javascript datomic-record))]
         home-route (reader/read-edn-string+ (:domain/home-route datomic-record))
         home-route (route/validate-route+ home-route)]
    (return (map->EdnishDomain
              {:ident (:domain/ident datomic-record)
               :fiddle-database (:domain/fiddle-database datomic-record)
               :databases (->> (:domain/databases datomic-record)
                               (map (juxt :domain.database/name :domain.database/record))
                               (into {}))
               :environment environment
               :home-route home-route
               :service-uri service-uri
               :build build}))))
