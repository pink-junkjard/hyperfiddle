(ns contrib.datomic-peer-test
  (:require
    [clojure.test :refer [deftest is]]
    [contrib.datomic-peer :refer [datomic-entity-successors clone-entities
                                  ref? many? one?]]
    [datomic.api :as d]
    [loom.alg-generic :as loom]))


(def uri "datomic:mem://empty")
(d/create-database uri)
(def $ (d/db (d/connect uri)))

(deftest schema-helpers-1
  []
  (is (= ((juxt #(ref? $ %)
                #(many? $ %)
                #(one? $ %))
           :db/ident)
         [false false true])))

(deftest datomic-graph-traverse-1
  []
  ; :db/ident's cardinality is the only reachable entity from :db/ident
  (is (= (->> (d/entity $ :db/ident)
              (loom/bf-traverse (partial datomic-entity-successors $))
              set
              count)
         1)))

(deftest clone-entities-1
  []
  (=
    (->> (d/q '[:find [?e ...] :where [?e :db/ident :db/ident]] $)
         (clone-entities $))
    [{:db/id "0",
      :db/ident :db.type/keyword,
      :fressian/tag :key,
      :db/doc "Value type for keywords. Keywords are used as names, and are interned for efficiency. Keywords map to the native interned-name type in languages that support them."}
     {:db/id "1",
      :db/ident :db.cardinality/one,
      :db/doc "One of two legal values for the :db/cardinality attribute. Specify :db.cardinality/one for single-valued attributes, and :db.cardinality/many for many-valued attributes."}
     {:db/id "2",
      :db/ident :db.unique/identity,
      :db/doc "Specifies that an attribute's value is unique. Attempts to create a new entity with a colliding value for a :db.unique/value will become upserts."}
     {:db/id "3",
      :db/ident :db/ident,
      :db/valueType {:db/id "0"},
      :db/cardinality {:db/id "1"},
      :db/unique {:db/id "2"},
      :db/doc "Attribute used to uniquely name an entity."}]))
