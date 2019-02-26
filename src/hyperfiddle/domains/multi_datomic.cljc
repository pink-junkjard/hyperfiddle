(ns hyperfiddle.domains.multi-datomic
  (:require
    [cats.core :as cats :refer [mlet]]
    [cats.monad.either :as either]
    [contrib.reader :as reader]
    [contrib.uri :refer [->URI]]
    [hypercrud.types.DbRef :refer [->DbRef]]
    [hypercrud.types.EntityRequest :refer [->EntityRequest]]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.domains.bidi :refer [map->BidiDomain]]
    [hyperfiddle.domains.ednish :refer [map->EdnishDomain]]
    [hyperfiddle.foundation :as foundation]
    [hyperfiddle.io.core :as io]
    [hyperfiddle.io.routes :as routes]
    [hyperfiddle.route :as route]
    [promesa.core :as p]))


(def database-pull
  [:db/id
   :database/uri
   :database.custom-security/client
   :database.custom-security/server
   {:database/write-security [:db/ident]}
   :hyperfiddle/owners])

(def domain-pull
  [:db/id
   :hyperfiddle/owners
   {:domain/databases [:domain.database/name
                       {:domain.database/record database-pull}]
    :domain/fiddle-database database-pull}
   :domain/disable-javascript
   :domain/environment
   :domain/ident
   :domain/router
   :domain/home-route])

(defn fiddle-dbname+ [datomic-record]
  (if-let [fiddledb-dbid (get-in datomic-record [:domain/fiddle-database :db/id])]
    (or (some #(when (= fiddledb-dbid (get-in % [:domain.database/record :db/id]))
                 (either/right (:domain.database/name %)))
              (:domain/databases datomic-record))
        (either/left (ex-info "Invalid :domain/fiddle-database. Must use a :domain/database"
                              (select-keys datomic-record [:domain/ident]))))
    (either/left (ex-info "Invalid :domain/fiddle-database. Missing :db/id"
                          (select-keys datomic-record [:domain/ident :domain/fiddle-database])))))

(defn hydrate-app-domain [io local-basis domain-eid service-uri build]
  (-> (io/hydrate-one! io local-basis nil (->EntityRequest domain-eid (->DbRef "$domains" foundation/root-branch) domain-pull))
      (p/then (fn [datomic-record]
                (if (nil? (:db/id datomic-record))
                  (p/rejected (ex-info "Domain not found" {:hyperfiddle.io/http-status-code 404}))
                  (-> (mlet [environment (reader/read-edn-string+ (:domain/environment datomic-record))
                             fiddle-dbname (fiddle-dbname+ datomic-record)
                             :let [partial-domain {:basis (get local-basis "$domains")
                                                   :ident (:domain/ident datomic-record)
                                                   :fiddle-dbname fiddle-dbname
                                                   :databases (->> (:domain/databases datomic-record)
                                                                   (map (juxt :domain.database/name :domain.database/record))
                                                                   (into {}))
                                                   :environment (assoc environment :domain/disable-javascript (:domain/disable-javascript datomic-record))
                                                   :service-uri service-uri
                                                   :build build}]]
                        (if (:domain/router datomic-record)
                          (->> (reader/read-edn-string+ (:domain/router datomic-record))
                               (cats/fmap (fn [router] (map->BidiDomain (assoc partial-domain :router router)))))
                          (->> (reader/read-edn-string+ (:domain/home-route datomic-record))
                               (cats/=<< route/validate-route+)
                               (cats/fmap (fn [home-route] (map->EdnishDomain (assoc partial-domain :home-route home-route)))))))
                      (either/branch p/rejected p/resolved)))))))

; app-domains = #{"bar.com"}
; fqdn = "foo.bar.com" or "myfancyfoo.com"
(defn domain-for-fqdn [io app-domains build protocol fqdn]
  (-> (io/sync io #{"$domains"})
      (p/then (fn [local-basis]
                (let [domain-eid (if-let [domain-ident (some #(second (re-find (re-pattern (str "^(.*)\\." % "$")) fqdn)) app-domains)]
                                   [:domain/ident domain-ident]
                                   [:domain/aliases fqdn])
                      service-uri (->URI (str protocol "://" fqdn))]
                  (hydrate-app-domain io local-basis domain-eid service-uri build))))))

(defn domains-domain [domains-transactor-uri service-uri build]
  (let [api-routes (routes/build build)]
    (reify domain/Domain
      (databases [domain] {"$domains" {:database/uri domains-transactor-uri}})
      (environment [domain] {})
      (api-routes [domain] api-routes)
      (service-uri [domain] service-uri))))
