(ns hypercrud.form.field
  (:require [hypercrud.form.option :as option]))


(defrecord Field [prompt
                  ident valueType cardinality isComponent   ;attribute
                  ;valid? required? read-only? default-value ;these should be functions that consume a record
                  options])


(defn convert-valueType [datomic-valueType]
  (condp = datomic-valueType
    :db.type/keyword :keyword
    :db.type/string :string
    :db.type/boolean :boolean
    :db.type/long :long
    :db.type/bigint :bigint
    :db.type/float :float
    :db.type/double :double
    :db.type/bigdec :bigdec
    :db.type/ref :ref
    :db.type/instant :instant
    :db.type/uuid :uuid
    :db.type/uri :uri
    :db.type/bytes :bytes))


(defn field
  ([{:keys [:field/prompt :field/label-prop :field/form :field/query] :as db-field}
    {:keys [:attribute/ident :attribute/valueType :attribute/cardinality :attribute/isComponent] :as db-attribute}]
   (->Field prompt ident (convert-valueType valueType) cardinality isComponent nil))

  ([{:keys [prompt ident valueType cardinality isComponent options]}]
   (->Field prompt ident valueType cardinality isComponent (option/gimme-useful-options options))))