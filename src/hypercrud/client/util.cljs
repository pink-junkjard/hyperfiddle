(ns hypercrud.client.util
  (:require [hypercrud.client.core :as hc]
            [hypercrud.util :as util]))


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
       (util/group-by-assume-unique :db/ident)))


(defn update-id-with-ident [entity graph attr]
  (util/update-existing entity attr #(:db/ident (hc/entity graph %))))
