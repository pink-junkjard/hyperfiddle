(ns hypercrud.client.util
  (:require [hypercrud.util :as util]))


(defn build-indexed-schema [schema]
  ;add system schema here
  (->> schema
       (concat [{:db/id 10 :db/ident :db/ident :db/valueType :db.type/keyword :db/cardinality :db.cardinality/one :db/unique :db.unique/identity :db/doc "Attribute used to uniquely name an entity."}
                {:db/id 11 :db/ident :db.install/partition :db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/doc "System attribute with type :db.type/ref. Asserting this attribute on :db.part/db with value v will install v as a partition."}
                {:db/id 12 :db/ident :db.install/valueType :db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/doc "System attribute with type :db.type/ref. Asserting this attribute on :db.part/db with value v will install v as a value type."}
                {:db/id 13 :db/ident :db.install/attribute, :db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/doc "System attribute with type :db.type/ref. Asserting this attribute on :db.part/db with value v will install v as an attribute."}
                {:db/id 14 :db/ident :db.install/function :db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/doc "System attribute with type :db.type/ref. Asserting this attribute on :db.part/db with value v will install v as a data function."}

                {:db/id 39 :db/ident :fressian/tag :db/valueType :db.type/keyword :db/cardinality :db.cardinality/one}
                {:db/id 40 :db/ident :db/valueType :db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
                {:db/id 41 :db/ident :db/cardinality :db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
                {:db/id 42 :db/ident :db/unique :db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
                {:db/id 43 :db/ident :db/isComponent :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}
                {:db/id 44 :db/ident :db/index :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}
                {:db/id 45 :db/ident :db/noHistory :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}
                ;todo 46-49 :db/lang etc
                {:db/id 50 :db/ident :db/txInstant :db/valueType :db.type/instant :db/cardinality :db.cardinality/one} ; tx entities
                {:db/id 51 :db/ident :db/fulltext :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}
                ;todo 52-55 :db/fn etc
                {:db/id 62 :db/ident :db/doc :db/valueType :db.type/string :db/cardinality :db.cardinality/one}])
       (util/group-by-assume-unique :db/ident)))
