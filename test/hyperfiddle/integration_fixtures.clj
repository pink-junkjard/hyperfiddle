(ns hyperfiddle.integration-fixtures
  (:require [contrib.uri :refer [->URI]]
            [hyperfiddle.database :as db]
            [hyperfiddle.io.transact :as transact]
            [datomic.api :as d]))


(def test-domains-uri (->URI "datomic:mem://test-domains"))
(def test-uri (->URI "datomic:mem://test"))

(defn domains-fixture [domains-owners]
  (fn [f]
    (db/provision-domains-db! test-domains-uri domains-owners)
    (try
      (f)
      (finally (db/deprovision-domains-db! test-domains-uri)))))

(defn db-fixture [uri db-owners subject & {:keys [schema security custom-security]}]
  (fn [f]
    (db/provision! uri db-owners test-domains-uri subject)
    (try
      (when security
        (transact/transact! test-domains-uri subject {test-domains-uri [(cond-> {:database/uri uri
                                                                                 :database/write-security security}
                                                                          (:client custom-security) (assoc :database.custom-security/client (:client custom-security))
                                                                          (:server custom-security) (assoc :database.custom-security/server (:server custom-security)))]}))
      (when schema
        (transact/transact! test-domains-uri subject {uri schema}))
      (f)
      (finally (db/deprovision! uri test-domains-uri subject)))))
