(ns contrib.datomic-tx-test
  (:require
    [clojure.set]
    [contrib.data]
    [contrib.datomic-tx]))


(def schema
  (->> [{:db/ident :foo
         :db/valueType :db.type/string
         :db/cardinality :db.cardinality/one}

        {:db/ident :bar
         :db/valueType :db.type/string
         :db/cardinality :db.cardinality/one}

        {:db/ident :ref
         :db/valueType :db.type/ref
         :db/cardinality :db.cardinality/one}

        {:db/ident :component
         :db/valueType :db.type/ref
         :db/cardinality :db.cardinality/one
         :db/isComponent true}]
       (contrib.data/group-by-unique :db/ident)))

(defmacro test-into-tx
  ([more-statements expected-out]
   (list 'test-into-tx [] more-statements expected-out))
  ([tx more-statements expected-out]
   (list 'test-into-tx schema tx more-statements expected-out))
  ([schema tx more-statements expected-out]
   `(let [out# (contrib.datomic-tx/into-tx ~schema ~tx ~more-statements)
          s-expected-out# (set ~expected-out)
          s-out# (set out#)]
      (is (~'= (count ~expected-out) (count out#)))
      (is (~'empty? (clojure.set/difference s-out# s-expected-out#)) "Unexpected datoms")
      (is (~'empty? (clojure.set/difference s-expected-out# s-out#)) "Missing datoms"))))
