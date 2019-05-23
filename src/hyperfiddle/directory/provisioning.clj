(ns hyperfiddle.directory.provisioning
  (:require
    [clojure.java.io :as io]
    [contrib.reader :as reader]
    [contrib.uri :refer [is-uri?]]
    [datomic.api :as d]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.directory.core :as directory]
    [hyperfiddle.io.datomic.peer :as peer]
    [hyperfiddle.io.datomic.transact :as transact]
    [hyperfiddle.security :as security]
    [hyperfiddle.security.domains]))


(defn build-util-domain [domains-uri]                       ; todo this is junk
  {:pre [(is-uri? domains-uri)]}
  (let [dbs (->> (d/connect (str domains-uri)) d/db
                 (d/q '[:find ?uri (pull ?db db-pull)
                        :in db-pull $
                        :where [?db :database/uri ?uri]]
                      directory/database-pull)
                 (into {}))]
    (reify domain/Domain
      (connect [domain dbname] (-> (domain/database domain dbname) :database/uri peer/connect))
      (databases [domain] dbs))))

(defn provision! [uri owners domains-uri subject]
  {:pre [(is-uri? uri)]}
  (let [domain (build-util-domain domains-uri)]             ; todo domains domain?
    (transact/transact! domain subject {domains-uri [{:database/uri uri
                                                      :database/write-security ::security/owner-only
                                                      :hyperfiddle/owners owners}]}))
  (when-not (d/create-database (str uri))
    ; security on domains should prevent this from ever happening
    (throw (ex-info "Database already exists" {:uri uri}))))

; todo drive both this AND prod from the same source, otherwise they will diverge over time
(def directory-schema (-> (io/resource "schema/directory.edn") slurp reader/read-edn-string!))

(defn provision-domains-db! [uri owners]
  (assert (d/create-database (str uri)) (str "Domains db already exists: " uri))
  (let [conn (d/connect (str uri))]
    @(d/transact conn directory-schema)
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
    (let [tx [[:db/retractEntity [:database/uri uri]]]
          domain (build-util-domain domains-uri)]
      (transact/transact! domain subject {domains-uri tx}))
    (throw (ex-info "Database already deleted" {:uri uri}))))

(defn deprovision-domains-db! [domains-uri]
  (d/delete-database (str domains-uri)))
