(ns hyperfiddle.integration-fixtures
  (:require [contrib.uri :refer [->URI]]
            [hyperfiddle.database :as db]
            [hyperfiddle.io.transact :as transact]))


(def test-domains-uri (->URI "datomic:mem://test-domains"))
(def test-uri (->URI "datomic:mem://test"))

(defn domains-fixture [domains-owners]
  (fn [f]
    (db/provision-domains-db! test-domains-uri domains-owners)
    (f)
    (db/deprovision-domains-db! test-domains-uri)))

(defn db-fixture [uri db-owners subject & {:keys [schema security custom-write-sec]}]
  (fn [f]
    (db/provision! uri db-owners test-domains-uri subject)
    (when schema
      (transact/transact! test-domains-uri subject {uri schema}))
    (when security
      (transact/transact! test-domains-uri subject {test-domains-uri [{:database/uri uri
                                                                       :database/write-security security
                                                                       :database/custom-write-sec (or custom-write-sec "")}]}))
    (f)
    (db/deprovision! uri test-domains-uri subject)))
