(ns hyperfiddle.ide.domain
  (:require
    [cats.core :as cats :refer [mlet return]]
    [cats.monad.either :as either]
    [cognitect.transit :as t]
    [contrib.reader :as reader]
    [contrib.uri :refer [->URI]]
    [hypercrud.browser.base :as base]
    [hypercrud.transit :as transit]
    [hypercrud.types.DbRef :refer [->DbRef]]
    [hypercrud.types.EntityRequest :refer [->EntityRequest]]
    [hyperfiddle.database.color :as color]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.domains.multi-datomic :as multi-datomic]
    [hyperfiddle.foundation :as foundation]
    [hyperfiddle.io.core :as io]
    [hyperfiddle.io.routes :as routes]
    [hyperfiddle.route :as route]
    [promesa.core :as p]))


(def user-dbname-prefix "$user.")

(defn build-routes [build]
  ["/" {"api/" {(str build "/") (routes/api nil)
                [[#"[^/]*" :build] "/"] {true :force-refresh}
                true :404}
        "api-user/" {(str build "/") (routes/api "user")
                     [[#"[^/]*" :build] "/"] {true :force-refresh}
                     true :404}
        "auth0" {:get :auth0-redirect
                 #".+" :404
                 true :405}
        true {:get :ssr
              true :405}}])

(defn nested-user-routes [build]
  ["/" {"api-user/" {(str build "/") (routes/api nil)
                     [[#"[^/]*" :build] "/"] {true :force-refresh}
                     true :404}
        ; user domain auth not implemented
        #_#_"auth0" {:get :auth0-redirect
                     #".+" :404
                     true :405}
        true {:get :ssr
              true :405}}])

(defrecord IdeDomain [basis ident fiddle-dbname databases environment home-route service-uri build user-domain-record
                      html-root-id]
  domain/Domain
  (basis [domain] basis)
  (ident [domain] ident)
  (fiddle-dbname [domain] fiddle-dbname)
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
            (route/url-encode home-route)))))

  (api-routes [domain] (build-routes build))
  (service-uri [domain] service-uri)
  )

(defn with-serializer [ide-domain]
  (->> (let [rep-fn #(-> (into {} %) (dissoc :hack-transit-serializer))]
         #(transit/encode % :type :json-verbose :opts {:handlers (assoc transit/write-handlers IdeDomain (t/write-handler (constantly "IdeDomain") rep-fn))}))
       (assoc ide-domain :hack-transit-serializer)))

(defn from-rep [rep] (-> (map->IdeDomain rep) with-serializer))

(defn build+ [domains-basis ide-datomic-record service-uri build user-datomic-record]
  (mlet [environment (reader/read-edn-string+ (:domain/environment ide-datomic-record))
         :let [environment (assoc environment :domain/disable-javascript (:domain/disable-javascript ide-datomic-record))]
         home-route (reader/read-edn-string+ (:domain/home-route ide-datomic-record))
         home-route (route/validate-route+ home-route)
         fiddle-dbname (multi-datomic/fiddle-dbname+ ide-datomic-record)]
    (return
      (-> {:basis domains-basis
           :ident (:domain/ident ide-datomic-record)
           :fiddle-dbname fiddle-dbname
           :databases (let [user-dbs (->> (:domain/databases user-datomic-record)
                                          (remove (fn [db]
                                                    (= (get-in db [:domain.database/record :db/id])
                                                       (get-in user-datomic-record [:domain/fiddle-database :db/id]))))
                                          (map (fn [db]
                                                 (-> db
                                                     (update :domain.database/record assoc
                                                             :database/auto-transact false
                                                             :database/color (color/color-for-name (:domain.database/name db)))
                                                     (update :domain.database/name #(str user-dbname-prefix %)))))
                                          (map (juxt :domain.database/name :domain.database/record))
                                          (into {}))
                            user-fiddle-dbname (some (fn [db]
                                                       (when (= (get-in db [:domain.database/record :db/id])
                                                                (get-in user-datomic-record [:domain/fiddle-database :db/id]))
                                                         (:domain.database/name db)))
                                                     (:domain/databases user-datomic-record))
                            ide-$ (assoc (:domain/fiddle-database user-datomic-record)
                                    :database/auto-transact false
                                    :database/color (color/color-for-name user-fiddle-dbname))
                            ide-dbs (-> (->> (concat (->> (:domain/databases ide-datomic-record)
                                                          (map (fn [db] (update db :domain.database/record assoc
                                                                                :database/auto-transact true
                                                                                :database/color "#777")))))
                                             (map (juxt :domain.database/name :domain.database/record))
                                             (into {}))
                                        (assoc "$" ide-$))]
                        (into user-dbs ide-dbs))
           :environment environment
           :home-route home-route
           :service-uri service-uri
           :build build
           ::user-dbname->ide (->> (:domain/databases user-datomic-record)
                                   (map (fn [db]
                                          [(:domain.database/name db)
                                           (if (= (get-in db [:domain.database/record :db/id])
                                                  (get-in user-datomic-record [:domain/fiddle-database :db/id]))
                                             "$"
                                             (str user-dbname-prefix (:domain.database/name db)))]))
                                   (into {}))
           ::user-domain-record user-datomic-record
           :html-root-id "ide-root"
           }
          map->IdeDomain
          with-serializer))))

; shitty code duplication because we cant pass our api-routes data structure as props (no regex equality)
(defrecord EdnishDomain [basis ident fiddle-dbname databases environment home-route service-uri build]
  domain/Domain
  (basis [domain] basis)
  (ident [domain] ident)
  (fiddle-dbname [domain] fiddle-dbname)
  (databases [domain] databases)
  (environment [domain] environment)
  (url-decode [domain s] (route/url-decode s home-route))
  (url-encode [domain route] (route/url-encode route home-route))
  (api-routes [domain] (nested-user-routes build))
  (service-uri [domain] service-uri)
  )

(defn build-user+
  ([ide-domain]
   (build-user+ ide-domain (::user-domain-record ide-domain)))
  ([ide-domain user-domain-record]                          ; this api is clumsy, should just use single arity (hydrate with ide) so bases are sensical
    ; shitty code duplication because we cant pass our api-routes data structure as props (no regex equality)
   (mlet [environment (reader/read-edn-string+ (:domain/environment user-domain-record))
          fiddle-dbname (multi-datomic/fiddle-dbname+ user-domain-record)
          :let [partial-domain {:basis (:basis ide-domain)
                                :ident (:domain/ident user-domain-record)
                                :fiddle-dbname fiddle-dbname
                                :databases (->> (:domain/databases user-domain-record)
                                                (map (juxt :domain.database/name :domain.database/record))
                                                (into {}))
                                :environment (assoc environment :domain/disable-javascript (:domain/disable-javascript user-domain-record))
                                :service-uri (:service-uri ide-domain)
                                :build (:build ide-domain)}]]
     (->> (reader/read-edn-string+ (:domain/home-route user-domain-record))
          (cats/=<< route/validate-route+)
          (cats/fmap (fn [home-route] (map->EdnishDomain (assoc partial-domain :home-route home-route))))))))

(defn hydrate-ide-domain [io local-basis app-domain-ident service-uri build]
  (let [requests [(->EntityRequest [:domain/ident "hyperfiddle"] (->DbRef "$domains" foundation/root-branch) multi-datomic/domain-pull)
                  (->EntityRequest [:domain/ident app-domain-ident] (->DbRef "$domains" foundation/root-branch) multi-datomic/domain-pull)]]
    (-> (io/hydrate-all-or-nothing! io local-basis nil requests)
        (p/then (fn [[ide-domain user-domain]]
                  (if (nil? (:db/id ide-domain))
                    (p/rejected (ex-info "IDE domain not found" {:hyperfiddle.io/http-status-code 404}))
                    (-> (build+ (get local-basis "$domains") ide-domain service-uri build user-domain)
                        (either/branch p/rejected p/resolved))))))))

; app-domains = #{"hyperfiddle.com"}
; ide-domains = #{"hyperfiddle.net"}
; fqdn = "foo.hyperfiddle.net" or "foo.hyperfiddle.com" or "myfancyfoo.com"
; todo app-domains and ide-domains can just be a regex with one capture group
(defn domain-for-fqdn [io app-domains ide-domains build protocol fqdn]
  (assert (first app-domains) "Ide service must have app-domains configured")
  (-> (io/sync io #{"$domains"})
      (p/then (fn [local-basis]
                (let [service-uri (->URI (str protocol "://" fqdn))]
                  (if-let [app-domain-ident (some #(second (re-find (re-pattern (str "^(.*)\\." % "$")) fqdn)) app-domains)]
                    (multi-datomic/hydrate-app-domain io local-basis [:domain/ident app-domain-ident] service-uri build)
                    (if-let [[app-domain-ident ide-domain] (->> ide-domains
                                                                (map #(re-pattern (str "^(.*)\\.(" % ")$")))
                                                                (some #(re-find % fqdn))
                                                                next)]
                      (if (= "www" app-domain-ident)        ; todo this check is NOT ide
                        (multi-datomic/hydrate-app-domain io local-basis [:domain/ident "www"] service-uri build)
                        (-> (hydrate-ide-domain io local-basis app-domain-ident service-uri build)
                            (p/then #(assoc %
                                       :hyperfiddle.ide/fqdn fqdn
                                       :ide-domain ide-domain
                                       :app-domain-ident app-domain-ident))))
                      (multi-datomic/hydrate-app-domain io local-basis [:domain/aliases fqdn] service-uri build))))))))
