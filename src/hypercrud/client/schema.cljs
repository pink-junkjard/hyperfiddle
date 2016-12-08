(ns hypercrud.client.schema
  (:require [hypercrud.types :refer [->DbVal]]
            [hypercrud.util :as util]))


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


(defn fixup-attribute [attribute]
  (reduce (fn [acc [attr-key v]]
            (if v (assoc acc (keyword "db" (name attr-key)) v)
                  acc))
          {}
          attribute))


(defn build-schema [attributes]
  (->> attributes
       (map (fn [attr]
              (let [attr #_(select-keys attr [:attribute/ident
                                              :attribute/valueType
                                              :attribute/cardinality
                                              :attribute/doc
                                              :attribute/unique
                                              :attribute/index
                                              :attribute/fulltext
                                              :attribute/isComponent
                                              :attribute/noHistory])
                    (->> (map (juxt identity #(get attr %))
                              [:attribute/ident
                               :attribute/valueType
                               :attribute/cardinality
                               :attribute/doc
                               :attribute/unique
                               :attribute/index
                               :attribute/fulltext
                               :attribute/isComponent
                               :attribute/noHistory])
                         (into {}))]
                (-> attr
                    (util/update-existing :attribute/valueType :db/ident)
                    (util/update-existing :attribute/cardinality :db/ident)
                    (util/update-existing :attribute/unique :db/ident)))))
       set
       (map fixup-attribute)
       build-indexed-schema))


(defn query-schema [root-dbval project-dbval]
  {project-dbval ['[:find ?attr :in $ :where [?attr :attribute/ident]]
                  {"$" root-dbval}
                  {"?attr" [root-dbval ['*
                                        {:attribute/valueType [:db/id :db/ident]}
                                        {:attribute/cardinality [:db/id :db/ident]}
                                        {:attribute/unique [:db/id :db/ident]}]]}]})