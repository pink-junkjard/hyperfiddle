(ns hyperfiddle.ide.domain
  (:require
    [cats.core :refer [mlet return]]
    [cats.monad.either :as either]
    [cognitect.transit :as t]
    [contrib.reader :as reader]
    [hypercrud.browser.base :as base]
    [hypercrud.transit :as transit]
    [hypercrud.types.DbRef :refer [->DbRef]]
    [hypercrud.types.EntityRequest :refer [->EntityRequest]]
    [hypercrud.types.QueryRequest :refer [->QueryRequest]]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.domains.multi-datomic :as multi-datomic]
    [hyperfiddle.io.core :as io]
    [hyperfiddle.route :as route]
    [promesa.core :as p]))


(defrecord IdeDomain [ident fiddle-database databases environment home-route]
  domain/Domain
  (ident [domain] ident)
  (fiddle-database [domain] fiddle-database)
  (databases [domain] databases)
  (environment [domain] environment)

  (url-decode [domain s]
    (let [[fiddle-ident :as route] (route/url-decode s home-route)]
      (if (and (keyword? fiddle-ident) (= "hyperfiddle.ide" (namespace fiddle-ident)))
        route
        (let [[user-fiddle user-datomic-args service-args fragment] route
              ide-fiddle :hyperfiddle.ide/edit
              ide-datomic-args (into [(base/legacy-fiddle-ident->lookup-ref user-fiddle)] user-datomic-args)]
          (route/canonicalize ide-fiddle ide-datomic-args service-args fragment)))))
  (url-encode [domain route]
    (if (not= :hyperfiddle.ide/edit (first route))
      (route/url-encode route home-route)
      (let [[ide-fiddle ide-datomic-args service-args fragment] route
            [user-fiddle-lookup-ref & user-datomic-args] ide-datomic-args
            user-fiddle (base/legacy-lookup-ref->fiddle-ident user-fiddle-lookup-ref)]
        (-> (route/canonicalize user-fiddle (vec user-datomic-args) service-args fragment)
            (route/url-encode home-route))))))

(defn with-serializer [ide-domain]
  (->> (let [rep-fn #(-> (into {} %) (dissoc :hack-transit-serializer))]
         #(transit/encode % :opts {:handlers (assoc transit/write-handlers IdeDomain (t/write-handler (constantly "IdeDomain") rep-fn))}))
       (assoc ide-domain :hack-transit-serializer)))

(defn from-rep [rep] (-> (map->IdeDomain rep) with-serializer))

(defn build+ [datomic-record]
  (mlet [environment (reader/read-edn-string+ (:domain/environment datomic-record))
         :let [environment (assoc environment :domain/disable-javascript (:domain/disable-javascript datomic-record))]
         home-route (reader/read-edn-string+ (:domain/home-route datomic-record))
         home-route (route/validate-route+ home-route)
         :let [databases (->> (:domain/databases datomic-record)
                              (map (juxt :domain.database/name :domain.database/record))
                              (into {}))
               domain (-> (->IdeDomain (:domain/ident datomic-record) (:domain/fiddle-database datomic-record) databases environment home-route)
                          with-serializer)]]
    (return domain)))

(defn hydrate-ide-domain [io local-basis app-domain-ident]
  (let [requests [(->EntityRequest [:domain/ident "hyperfiddle"] (->DbRef "$domains" nil) multi-datomic/domain-pull)
                  (->QueryRequest '[:find (pull ?db [:database/uri
                                                     :database.custom-security/client
                                                     {:database/write-security [:db/ident]}
                                                     :hyperfiddle/owners]) .
                                    :in $ ?ident :where
                                    [?e :domain/ident ?ident]
                                    [?e :domain/fiddle-database ?db]]
                                  [(->DbRef "$domains" nil)
                                   app-domain-ident])]]
    (-> (io/hydrate-all-or-nothing! io local-basis nil requests)
        (p/then (fn [[ide-domain app-fiddle-db]]
                  (if (nil? (:db/id ide-domain))
                    (p/rejected (ex-info "IDE domain not found" {:hyperfiddle.io/http-status-code 404}))
                    (-> ide-domain
                        (update :domain/databases
                                (fn [dbs]
                                  (->> dbs
                                       (remove #(= "$" (:domain.database/name %)))
                                       (cons {:domain.database/name "$"
                                              :domain.database/record app-fiddle-db})
                                       vec)))
                        build+
                        (either/branch p/rejected p/resolved))))))))

; app-domains = #{"hyperfiddle.com"}
; ide-domains = #{"hyperfiddle.net"}
; fqdn = "foo.hyperfiddle.net" or "foo.hyperfiddle.com" or "myfancyfoo.com"
; todo app-domains and ide-domains can just be a regex with one capture group
(defn domain-for-fqdn [io app-domains ide-domains fqdn]
  (assert (first app-domains) "Ide service must have app-domains configured")
  (-> (io/sync io #{"$domains"})
      (p/then (fn [local-basis]
                (if-let [app-domain-ident (some #(second (re-find (re-pattern (str "^(.*)\\." % "$")) fqdn)) app-domains)]
                  (multi-datomic/hydrate-app-domain io local-basis [:domain/ident app-domain-ident])
                  (if-let [[app-domain-ident ide-domain] (->> ide-domains
                                                              (map #(re-pattern (str "^(.*)\\.(" % ")$")))
                                                              (some #(re-find % fqdn))
                                                              rest)]
                    (if (= "www" app-domain-ident)          ; todo this check is NOT ide
                      (multi-datomic/hydrate-app-domain io local-basis [:domain/ident "www"])
                      (-> (hydrate-ide-domain io local-basis app-domain-ident)
                          (p/then #(assoc %
                                     ; todo canonical app-host
                                     :hyperfiddle.ide/app-fqdn (str "http://" app-domain-ident "." (first app-domains))
                                     :hyperfiddle.ide/fqdn fqdn
                                     :ide-domain ide-domain
                                     :app-domain-ident app-domain-ident))))
                    (multi-datomic/hydrate-app-domain io local-basis [:domain/aliases fqdn])))))))
