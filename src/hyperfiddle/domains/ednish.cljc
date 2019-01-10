(ns hyperfiddle.domains.ednish
  (:require
    [cats.core :refer [mlet return]]
    [contrib.reader :as reader]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.route :as route]))


(defrecord EdnishDomain [ident fiddle-database databases environment home-route]
  domain/Domain
  (ident [domain] ident)
  (fiddle-database [domain] fiddle-database)
  (databases [domain] databases)
  (environment [domain] environment)
  (url-decode [domain s] (route/url-decode s home-route))
  (url-encode [domain route] (route/url-encode route home-route))
  )

(defn build+ [datomic-record]
  (mlet [environment (reader/read-edn-string+ (:domain/environment datomic-record))
         :let [environment (assoc environment :domain/disable-javascript (:domain/disable-javascript datomic-record))]
         home-route (reader/read-edn-string+ (:domain/home-route datomic-record))
         home-route (route/validate-route+ home-route)
         :let [databases (->> (:domain/databases datomic-record)
                              (map (juxt :domain.database/name :domain.database/record))
                              (into {}))]]
    (return (->EdnishDomain (:domain/ident datomic-record) (:domain/fiddle-database datomic-record) databases environment home-route))))
