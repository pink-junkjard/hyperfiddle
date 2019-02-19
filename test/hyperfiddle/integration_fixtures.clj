(ns hyperfiddle.integration-fixtures
  (:require
    [contrib.uri :refer [->URI]]
    [datomic.api :as d]
    [hyperfiddle.directory.provisioning :as dp]
    [hyperfiddle.io.datomic.transact :as transact]))


(def test-domains-uri (->URI "datomic:mem://test-domains"))
(def test-uri (->URI "datomic:mem://test"))

(defn domains-fixture [domains-owners]
  (fn [f]
    (dp/provision-domains-db! test-domains-uri domains-owners)
    (try
      (f)
      (finally (dp/deprovision-domains-db! test-domains-uri)))))

(defn db-fixture [uri db-owners subject & {:keys [schema security custom-security]}]
  (fn [f]
    (dp/provision! uri db-owners test-domains-uri subject)
    (try
      (when security
        (let [domain (dp/build-util-domain test-domains-uri)
              tx-groups {test-domains-uri [(cond-> {:database/uri uri
                                                    :database/write-security security}
                                             (:client custom-security) (assoc :database.custom-security/client (:client custom-security))
                                             (:server custom-security) (assoc :database.custom-security/server (:server custom-security)))]}]
          (transact/transact! domain subject tx-groups)))
      (when schema
        (let [domain (dp/build-util-domain test-domains-uri)] ; rebuild because we just altered
          (transact/transact! domain subject {uri schema})))
      (f)
      (finally (dp/deprovision! uri test-domains-uri subject)))))
