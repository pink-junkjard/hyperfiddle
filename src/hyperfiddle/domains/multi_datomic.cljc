(ns hyperfiddle.domains.multi-datomic
  (:require
    [bidi.bidi :as bidi]
    [cats.monad.either :as either]
    [contrib.uri :refer [->URI]]
    [hypercrud.types.DbRef :refer [->DbRef]]
    [hypercrud.types.EntityRequest :refer [->EntityRequest]]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.domains.bidi :as bidi-domain]
    [hyperfiddle.domains.ednish :as ednish-domain]
    [hyperfiddle.io.core :as io]
    [hyperfiddle.io.routes :as routes]
    [promesa.core :as p]))


(def database-pull
  [:database/uri
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

(defn hydrate-app-domain [io local-basis domain-eid service-uri build]
  (-> (io/hydrate-one! io local-basis nil (->EntityRequest domain-eid (->DbRef "$domains" nil) domain-pull))
      (p/then (fn [datomic-record]
                (if (nil? (:db/id datomic-record))
                  (p/rejected (ex-info "Domain not found" {:hyperfiddle.io/http-status-code 404}))
                  (-> (if (:domain/router datomic-record)
                        (bidi-domain/build+ datomic-record service-uri build)
                        (ednish-domain/build+ datomic-record service-uri build))
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
