(ns hyperfiddle.domains.multi-datomic
  (:require
    [cats.monad.either :as either]
    [hypercrud.types.DbRef :refer [->DbRef]]
    [hypercrud.types.EntityRequest :refer [->EntityRequest]]
    [hyperfiddle.domains.bidi :as bidi-domain]
    [hyperfiddle.domains.ednish :as ednish-domain]
    [hyperfiddle.io.core :as io]
    [promesa.core :as p]))


(def domain-pull
  [:db/id
   :hyperfiddle/owners
   {:domain/databases [:domain.database/name
                       {:domain.database/record [:database/uri
                                                 :database.custom-security/client
                                                 {:database/write-security [:db/ident]}
                                                 :hyperfiddle/owners]}]
    :domain/fiddle-database [:database/uri
                             :database.custom-security/client
                             {:database/write-security [:db/ident]}
                             :hyperfiddle/owners]}
   :domain/disable-javascript
   :domain/environment
   :domain/ident
   :domain/router
   :domain/home-route])

(defn hydrate-app-domain [io local-basis domain-eid]
  (-> (io/hydrate-one! io local-basis nil (->EntityRequest domain-eid (->DbRef "$domains" nil) domain-pull))
      (p/then (fn [datomic-record]
                (if (nil? (:db/id datomic-record))
                  (p/rejected (ex-info "Domain not found" {:hyperfiddle.io/http-status-code 404}))
                  (-> (if (:domain/router datomic-record)
                        (bidi-domain/build+ datomic-record)
                        (ednish-domain/build+ datomic-record))
                      (either/branch p/rejected p/resolved)))))))

; app-domains = #{"bar.com"}
; fqdn = "foo.bar.com" or "myfancyfoo.com"
(defn domain-for-fqdn [io app-domains fqdn]
  (-> (io/sync io #{"$domains"})
      (p/then (fn [local-basis]
                (let [domain-eid (if-let [domain-ident (some #(second (re-find (re-pattern (str "^(.*)\\." % "$")) fqdn)) app-domains)]
                                   [:domain/ident domain-ident]
                                   [:domain/aliases fqdn])]
                  (hydrate-app-domain io local-basis domain-eid))))))
