(ns hyperfiddle.ide
  (:require
    [clojure.java.io :as io]
    [contrib.reader :as reader]
    [hyperfiddle.io.datomic :as d]))


(defn transact-ide! [datomic conn]
  (let [ide-schema (-> (io/resource "schema/fiddle.edn") slurp reader/read-edn-string!)
        ide-data (-> (io/resource "ide.edn") slurp reader/read-edn-string!)]
    (d/transact datomic conn {:tx-data ide-schema})
    (d/transact datomic conn {:tx-data ide-data})))
