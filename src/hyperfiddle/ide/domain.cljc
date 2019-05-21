(ns hyperfiddle.ide.domain
  (:require
    [cats.core :as cats]
    [cats.monad.either :as either]
    [clojure.spec.alpha :as s]
    [cognitect.transit :as t]
    [contrib.uri :refer [is-uri?]]
    [hypercrud.browser.base :as base]
    [hypercrud.transit :as transit]
    [hyperfiddle.database.color :as color]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.domains.ednish :as ednish-domain :refer [#?(:cljs EdnishDomain)]]
    [hyperfiddle.ide.system-fiddle :as ide-system-fiddle]
    [hyperfiddle.io.routes :as routes]
    [hyperfiddle.route :as route]
    [hyperfiddle.system-fiddle :as system-fiddle])
  #?(:clj
     (:import
       (hyperfiddle.domains.ednish EdnishDomain))))


(def user-dbname-prefix "$user.")

(def routes
  ["/" {"api/" {(str routes/version "/") (routes/api nil)
                [[#"[^/]*" :version] "/"] {true :force-refresh}
                true :404}
        "api-user/" {(str routes/version "/") (routes/api "user")
                     [[#"[^/]*" :version] "/"] {true :force-refresh}
                     true :404}
        "auth0" {:get :hyperfiddle.ide/auth0-redirect
                 #".+" :404
                 true :405}
        "logout" {:post :hyperfiddle.ide/logout
                  #".+" :404
                  true :405}
        "static/" {[:build "/" :resource-name] {:get :static-resource
                                                true :405}
                   true :404}
        "favicon.ico" :favicon
        true {:get :ssr
              true :405}}])

(def nested-user-routes
  ["/" {"api-user/" {(str routes/version "/") (routes/api nil)
                     [[#"[^/]*" :version] "/"] {true :force-refresh}
                     true :404}
        ; todo this static path conflicts with the ide
        "static/" {[:build "/" :resource-name] {:get :static-resource
                                                true :405}
                   true :404}
        true {:get :ssr
              true :405}}])

(defrecord IdeDomain [basis fiddle-dbname databases environment home-route build
                      html-root-id]
  domain/Domain
  (basis [domain] basis)
  (type-name [domain] (str *ns* "/" "IdeDomain"))
  (fiddle-dbname [domain] fiddle-dbname)
  (databases [domain] databases)
  (environment [domain] environment)

  (url-decode [domain s]
    (let [[fiddle-ident :as route] (route/url-decode s home-route)]
      (if (and (keyword? fiddle-ident) (#{"hyperfiddle.ide" "hyperfiddle.ide.schema"} (namespace fiddle-ident)))
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

  (api-routes [domain] routes)
  (system-fiddle? [domain fiddle-ident]
    (or (and (keyword? fiddle-ident) (= "hyperfiddle.ide.schema" (namespace fiddle-ident)))
        (system-fiddle/system-fiddle? fiddle-ident)))
  (hydrate-system-fiddle [domain fiddle-ident]
    (if (and (keyword? fiddle-ident) (= "hyperfiddle.ide.schema" (namespace fiddle-ident)))
      (ide-system-fiddle/hydrate fiddle-ident (::user-dbname->ide domain))
      (system-fiddle/hydrate fiddle-ident)))
  )

; shitty code duplication because we cant pass our api-routes data structure as props (no regex equality)
(defrecord IdeEdnishDomain [basis fiddle-dbname databases environment home-route]
  domain/Domain
  (basis [domain] basis)
  (type-name [domain] (str *ns* "/" "IdeEdnishDomain"))
  (fiddle-dbname [domain] fiddle-dbname)
  (databases [domain] databases)
  (environment [domain] environment)
  (url-decode [domain s] (route/url-decode s home-route))
  (url-encode [domain route] (route/url-encode route home-route))
  (api-routes [domain] nested-user-routes)
  (system-fiddle? [domain fiddle-ident] (system-fiddle/system-fiddle? fiddle-ident))
  (hydrate-system-fiddle [domain fiddle-ident] (system-fiddle/hydrate fiddle-ident))
  )

(defn with-serializer [ide-domain]
  (->> (let [rep-fn #(-> (into {} %) (dissoc :hack-transit-serializer))]
         (fn [domain]
           (-> domain
               (update ::user-domain+ #(cats/fmap (partial into {}) %))
               (transit/encode :opts {:handlers (assoc transit/write-handlers
                                                  IdeDomain (t/write-handler (constantly "IdeDomain") rep-fn))}))))
       (assoc ide-domain :hack-transit-serializer)))

(defn from-rep [rep]
  (-> (map->IdeDomain rep)
      (update ::user-domain+ #(cats/fmap map->IdeEdnishDomain %))
      with-serializer))

(defn build
  [basis
   user-databases user-fiddle-dbname user-domain+
   ide-databases ide-fiddle-dbname
   & {:keys [ide-environment ide-home-route]
      :or {ide-environment {}
           ide-home-route [:hyperfiddle.ide/home]}}]
  (-> {:basis basis
       :fiddle-dbname ide-fiddle-dbname
       :databases (let [user-dbs (->> (dissoc user-databases user-fiddle-dbname)
                                      (map (fn [[dbname db]]
                                             [(str user-dbname-prefix dbname)
                                              (assoc db
                                                :database/auto-transact false
                                                :database/color (color/color-for-name dbname))]))
                                      (into {}))
                        ide-dbs (->> ide-databases
                                     (map (fn [[dbname db]]
                                            [dbname
                                             (assoc db
                                               :database/auto-transact true
                                               :database/color "#777")]))
                                     (into {}))]
                    (-> (into user-dbs ide-dbs)
                        (assoc "$" (assoc (get user-databases user-fiddle-dbname)
                                     :database/auto-transact false
                                     :database/color (color/color-for-name user-fiddle-dbname)))))
       :environment ide-environment
       :home-route ide-home-route
       ::user-dbname->ide (->> user-databases
                               (map (fn [[dbname db]]
                                      [dbname
                                       (if (= user-fiddle-dbname dbname)
                                         "$"
                                         (str user-dbname-prefix dbname))]))
                               (into {}))
       ::user-domain+ user-domain+
       :html-root-id "ide-root"}
      map->IdeDomain
      with-serializer))

(defn build-from-user-domain
  ([user-domain $src-uri-or-db-name]
   (build-from-user-domain user-domain $src-uri-or-db-name {}))
  ([user-domain $src-uri-or-db-name ide-environment & {:as ide-databases}]
   {:pre [(instance? EdnishDomain user-domain)
          (s/valid? ednish-domain/spec user-domain)
          (s/valid? (s/nilable :hyperfiddle.domain/databases) ide-databases)]}
   (build
     (domain/basis user-domain)
     (domain/databases user-domain)
     (domain/fiddle-dbname user-domain)
     (-> user-domain map->IdeEdnishDomain either/right)
     (into {"$src" (if (is-uri? $src-uri-or-db-name)
                     {:database/uri $src-uri-or-db-name}
                     {:database/db-name $src-uri-or-db-name})}
           ide-databases)
     "$src"
     :ide-environment ide-environment)))
