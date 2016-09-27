(ns hypercrud.client.util
  (:require [hypercrud.client.core :as hc]))


(defn map-values [f m]
  (->> (map (fn [[k v]] [k (f v)]) m)
       (into {})))


(defn group-by-assume-unique [f xs]
  (into {} (map (fn [x] [(f x) x]) xs)))


(defn build-indexed-schema [schema]
  ;add system schema here
  (->> schema
       (concat [{:db/ident :db/ident :db/valueType :db.type/keyword :db/cardinality :db.cardinality/one}
                {:db/ident :db/valueType :db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
                {:db/ident :db/cardinality :db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
                {:db/ident :db/doc :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
                {:db/ident :db/unique :db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
                {:db/ident :db/index :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}
                {:db/ident :db/fulltext :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}
                {:db/ident :db/isComponent :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}
                {:db/ident :db/noHistory :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}

                {:db/ident :fressian/tag :db/valueType :db.type/keyword :db/cardinality :db.cardinality/one}])
       (group-by-assume-unique :db/ident)))


(defn update-existing [m k f]
  (if (get m k)
    (update m k f)
    m))


(defn update-id-with-ident [entity graph attr]
  (update-existing entity attr #(:db/ident (hc/entity graph %))))
