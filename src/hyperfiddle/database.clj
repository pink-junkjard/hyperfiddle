(ns hyperfiddle.database
  (:require [datomic.api :as d]
            [hyperfiddle.io.transact :as transact]
            [hyperfiddle.security :as security]
            [hyperfiddle.security.domains]))


(defn provision! [uri owners domains-uri subject]
  (transact/transact! domains-uri subject {domains-uri [{:database/uri uri
                                                         :database/write-security ::security/owner-only
                                                         :hyperfiddle/owners owners}]})
  (when-not (d/create-database (str uri))
    ; security on domains should prevent this from ever happening
    (throw (ex-info "Database already exists" {:uri uri}))))

; todo drive both this AND prod from the same source, otherwise they will diverge over time
(def domains-schema
  [{:db/ident :database/uri :db/valueType :db.type/uri :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
   {:db/ident :database/write-security :db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
   {:db/ident :database.custom-security/client :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
   {:db/ident :database.custom-security/server :db/valueType :db.type/string :db/cardinality :db.cardinality/one}

   {:db/ident :hyperfiddle.security/owner-only}
   {:db/ident :hyperfiddle.security/authenticated-users-only}
   {:db/ident :hyperfiddle.security/allow-anonymous}
   {:db/ident :hyperfiddle.security/custom}

   {:db/ident :hyperfiddle/owners :db/valueType :db.type/uuid :db/cardinality :db.cardinality/many}

   {:db/ident :domain/aliases :db/valueType :db.type/string :db/cardinality :db.cardinality/many :db/unique :db.unique/value :db/doc "Point your production DNS at the hyperfiddle.net IP and register the domain name here. Aliases are served without the dev toolbar."}
   {:db/ident :domain/code :db/valueType :db.type/string :db/cardinality :db.cardinality/one :db/doc "a ClojureScript form for storing view functions, evaluated on page load"}
   {:db/ident :domain/css :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
   {:db/ident :domain/disable-javascript :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one :db/doc "Elide Hyperfiddle javascript in production domains so client doesn't have to parse/eval it, this will decrease time-to-interaction on static sites like blogs. Counter-intuitively this will probably make your app overall slower because without javascript you can't take advantage of `Cache-control: Immutable` on api responses which get the entire static site in browser cache."}
   {:db/ident :domain/environment :db/valueType :db.type/string :db/cardinality :db.cardinality/one :db/doc "EDN map. Keys starting with `$` name datomic databases as seen from `datomic.api/q`, and their value must be a reachable datomic uri. Other entries are constants available to your fiddles, for example third-party API keys."}
   {:db/ident :domain/fiddle-database :db/valueType :db.type/ref :db/cardinality :db.cardinality/one :db/doc "Datomic database to store fiddle data. It can be the same as your environment databases."}
   {:db/ident :domain/databases :db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   {:db/ident :domain.database/name :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
   {:db/ident :domain.database/record :db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
   {:db/ident :domain/home-route :db/valueType :db.type/string :db/cardinality :db.cardinality/one :db/doc "Index route for this domain, it can have parameters"}
   {:db/ident :domain/ident :db/valueType :db.type/string :db/cardinality :db.cardinality/one :db/unique :db.unique/identity :db/doc "Your subdomain. Use this uri to access your fiddles in dev mode."}
   {:db/ident :domain/router :db/valueType :db.type/string :db/cardinality :db.cardinality/one :db/doc "Experimental and undocumented userland router definition"}])

(defn provision-domains-db! [uri owners]
  (assert (d/create-database (str uri)) (str "Domains db already exists: " uri))
  (let [conn (d/connect (str uri))]
    @(d/transact conn domains-schema)
    @(d/transact conn [{:database/uri uri
                        :database/write-security ::security/custom
                        :database.custom-security/client (str `hyperfiddle.security.domains/client)
                        :database.custom-security/server (str `hyperfiddle.security.domains/server)
                        :hyperfiddle/owners owners}])))

(defn deprovision! [uri domains-uri subject]
  (let [db (d/db (d/connect (str domains-uri)))
        owners (into #{security/root} (:hyperfiddle/owners (d/entity db [:database/uri uri])))]
    (when-not (contains? owners subject)
      (throw (security/tx-validation-failure))))

  (if (d/delete-database (str uri))
    (let [tx [[:db/retractEntity [:database/uri uri]]]]
      (transact/transact! domains-uri subject {domains-uri tx}))
    (throw (ex-info "Database already deleted" {:uri uri}))))

(defn deprovision-domains-db! [domains-uri]
  (d/delete-database (str domains-uri)))
