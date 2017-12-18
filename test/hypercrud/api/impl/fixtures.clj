(ns hypercrud.api.impl.fixtures
  (:require [datomic.api :as d]))


(def test-uri (str "datomic:mem://test"))

(defn setup [schema]
  (d/create-database test-uri)
  (d/transact (d/connect test-uri) schema))

(defn tear-down []
  (d/delete-database test-uri))

(defn build-fixtures [schema]
  (fn [f]
    (setup schema)
    (f)
    (tear-down)))
