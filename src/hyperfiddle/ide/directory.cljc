(ns hyperfiddle.ide.directory
  (:require
    [cats.core :as cats :refer [mlet return]]
    [cats.monad.either :as either]
    [contrib.reader :as reader]
    [hypercrud.types.DbRef :refer [->DbRef]]
    [hypercrud.types.EntityRequest :refer [->EntityRequest]]
    [hyperfiddle.directory.core :as directory]
    [hyperfiddle.foundation :as foundation]
    [hyperfiddle.ide.domain :as ide-domain]
    [hyperfiddle.io.core :as io]
    [hyperfiddle.route :as route]
    [promesa.core :as p]))


(defn- build-user+ [basis user-domain-record]
  ; shitty code duplication because we cant pass our api-routes data structure as props (no regex equality)
  (mlet [environment (reader/read-edn-string+ (:domain/environment user-domain-record))
         fiddle-dbname (directory/fiddle-dbname+ user-domain-record)
         :let [partial-domain {:basis basis
                               :fiddle-dbname fiddle-dbname
                               :databases (->> (:domain/databases user-domain-record)
                                               (map (juxt :domain.database/name :domain.database/record))
                                               (into {}))
                               :environment (assoc environment :domain/disable-javascript (:domain/disable-javascript user-domain-record))}]]
    (->> (reader/read-edn-string+ (:domain/home-route user-domain-record))
         (cats/=<< route/validate-route+)
         (cats/fmap (fn [home-route] (ide-domain/map->IdeEdnishDomain (assoc partial-domain :home-route home-route)))))))

(defn- build+ [domains-basis ide-datomic-record user-datomic-record]
  (mlet [environment (reader/read-edn-string+ (:domain/environment ide-datomic-record))
         :let [environment (assoc environment :domain/disable-javascript (:domain/disable-javascript ide-datomic-record))]
         home-route (reader/read-edn-string+ (:domain/home-route ide-datomic-record))
         home-route (route/validate-route+ home-route)
         fiddle-dbname (directory/fiddle-dbname+ ide-datomic-record)
         :let [user-fiddle-dbname (either/branch
                                    (directory/fiddle-dbname+ user-datomic-record)
                                    (constantly nil)        ; user domains can be misconfigured, can just leave ide-$ unbound
                                    identity)]]
    (return
      (ide-domain/build
        domains-basis
        (->> (:domain/databases user-datomic-record)
             (map (juxt :domain.database/name :domain.database/record))
             (into {}))
        user-fiddle-dbname
        (build-user+ domains-basis user-datomic-record)
        (->> (:domain/databases ide-datomic-record)
             (map (juxt :domain.database/name :domain.database/record))
             (into {}))
        fiddle-dbname
        :ide-environment environment
        :ide-home-route home-route))))

(defn- hydrate-ide-domain [io local-basis app-domain-ident]
  (let [requests [(->EntityRequest [:domain/ident "hyperfiddle"] (->DbRef "$domains" foundation/root-branch) directory/domain-pull)
                  (->EntityRequest [:domain/ident app-domain-ident] (->DbRef "$domains" foundation/root-branch) directory/domain-pull)]]
    (-> (io/hydrate-all-or-nothing! io local-basis nil requests)
        (p/then (fn [[ide-domain user-domain]]
                  (cond
                    (nil? (:db/id ide-domain)) (p/rejected (ex-info "IDE misconfigured; ide domain not found" {:hyperfiddle.io/http-status-code 500}))
                    (nil? (:db/id user-domain)) (p/rejected (ex-info "Domain not found" {:hyperfiddle.io/http-status-code 404}))
                    :else (-> (build+ (get local-basis "$domains") ide-domain user-domain)
                              (either/branch p/rejected p/resolved))))))))

; app-domains = #{"hyperfiddle.com"}
; ide-domains = #{"hyperfiddle.net"}
; fqdn = "foo.hyperfiddle.net" or "foo.hyperfiddle.com" or "myfancyfoo.com"
; todo app-domains and ide-domains can just be a regex with one capture group
(defn build-domain-provider [io app-domains ide-domains]
  (assert (first app-domains) "Ide service must have app-domains configured")
  (fn [fqdn]
    (-> (io/sync io #{"$domains"})
        (p/then (fn [local-basis]
                  (if-let [app-domain-ident (some #(second (re-find (re-pattern (str "^(.*)\\." % "$")) fqdn)) app-domains)]
                    (directory/hydrate-app-domain io local-basis [:domain/ident app-domain-ident])
                    (if-let [[app-domain-ident ide-domain] (->> ide-domains
                                                                (map #(re-pattern (str "^(.*)\\.(" % ")$")))
                                                                (some #(re-find % fqdn))
                                                                next)]
                      (if (= "www" app-domain-ident)        ; todo this check is NOT ide
                        (directory/hydrate-app-domain io local-basis [:domain/ident "www"])
                        (-> (hydrate-ide-domain io local-basis app-domain-ident)
                            (p/then #(assoc %
                                       ::fqdn fqdn
                                       ::ide-domain ide-domain
                                       ::app-domain-ident app-domain-ident

                                       ; legacy
                                       :ide-domain ide-domain
                                       :app-domain-ident app-domain-ident))))
                      (directory/hydrate-app-domain io local-basis [:domain/aliases fqdn]))))))))
