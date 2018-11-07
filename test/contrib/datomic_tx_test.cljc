(ns contrib.datomic-tx-test
  #?(:cljs (:require-macros [contrib.datomic-tx-test :refer [test-into-tx]]))
  (:require
    [contrib.data :as data]
    [contrib.datomic-tx :refer [edit-entity into-tx]]
    [clojure.set :as set]
    [clojure.test :refer [deftest is testing]]))


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
       (data/group-by-unique :db/ident)))

#?(:clj
   (defmacro test-into-tx
     ([more-statements expected-out]
      (list 'test-into-tx [] more-statements expected-out))
     ([tx more-statements expected-out]
      (list 'test-into-tx schema tx more-statements expected-out))
     ([schema tx more-statements expected-out]
      `(let [out# (into-tx ~schema ~tx ~more-statements)
             s-expected-out# (set ~expected-out)
             s-out# (set out#)]
         (is (~'= (count ~expected-out) (count out#)))
         (is (~'empty? (set/difference s-out# s-expected-out#)) "Unexpected datoms")
         (is (~'empty? (set/difference s-expected-out# s-out#)) "Missing datoms")))))

(deftest no-op []
  (test-into-tx [] []))

(deftest add-one []
  (test-into-tx
    [[:db/add 1 :district/name "Southwest"]
     [:db/add 1 :district/region 2]]
    [[:db/add 1 :district/name "Southwest"]
     [:db/add 1 :district/region 2]]
    ))

(deftest add-one-override-prior-matching-attr []
  (test-into-tx
    [[:db/add 1 :district/region 2]
     [:db/add 1 :district/name "Southwest"]
     [:db/retract 1 :district/name "Southwest"]
     [:db/add 1 :district/name ""]]
    [[:db/add 1 :district/region 2]
     [:db/add 1 :district/name ""]]
    ))

(deftest retract-one-cancel-matching-add []
  (test-into-tx
    [[:db/add 1 :district/name "Southwest"]
     [:db/add 1 :district/region 2]
     [:db/retract 1 :district/region 2]]
    [[:db/add 1 :district/name "Southwest"]])
  (test-into-tx
    [[:db/add 1 :district/name "Southwest"]
     [:db/retract 1 :district/name "Southwest"]]
    [])
  (test-into-tx
    [[:db/retract 1 :district/name "Southwest"]
     [:db/add 1 :district/name "Southwest"]]
    []))

(deftest retract-one-remove-when-not-exists-preserve-retract []
  (test-into-tx
    [[:db/retract 1 :district/region 2]]
    [[:db/retract 1 :district/region 2]]))

(deftest add-many-add-to-set []
  (test-into-tx
    [[:db/add 1 :community/type 20]
     [:db/add 1 :community/type 21]]
    [[:db/add 1 :community/type 20]
     [:db/add 1 :community/type 21]]))

(deftest retract-many-cancel-matching-add []
  (test-into-tx
    [[:db/add 1 :community/type 20]
     [:db/add 1 :community/type 21]
     [:db/retract 1 :community/type 21]]
    [[:db/add 1 :community/type 20]])
  (test-into-tx
    [[:db/add 1 :community/type 20]
     [:db/retract 1 :community/type 20]]
    [])
  (test-into-tx
    [[:db/retract 1 :community/type 20]
     [:db/add 1 :community/type 20]]
    []))

(deftest retract-many-empty-entity-preserve-retract []
  (test-into-tx
    [[:db/retract 1 :community/type 20]]
    [[:db/retract 1 :community/type 20]]))

(deftest add-many-cancel-matching-retract []
  (test-into-tx
    [[:db/add 1 :community/type 20]
     [:db/retract 1 :community/type 21]
     [:db/add 1 :community/type 21]]
    [[:db/add 1 :community/type 20]]))

(deftest longer-test-one []
  (test-into-tx
    [[:db/add 1 :district/region 2]
     [:db/add 1 :district/name "Southwest"]
     [:db/add 2 :community/name "Asdf"]
     [:db/add 2 :community/url "asdf.com"]
     [:db/retract 1 :district/name "Southwest"]
     [:db/add 1 :district/name ""]]
    [[:db/add 1 :district/region 2]
     [:db/add 2 :community/name "Asdf"]
     [:db/add 2 :community/url "asdf.com"]
     [:db/add 1 :district/name ""]]))

(deftest longer-test-many []
  (test-into-tx
    [[:db/add 1 :community/type 2]
     [:db/add 1 :community/type 2]]
    [[:db/add 1 :community/type 2]])
  (test-into-tx
    [[:db/add 1 :community/type 2]
     [:db/add 1 :community/type 2]
     [:db/retract 1 :community/type 2]]
    [])
  (test-into-tx
    [[:db/retract 1 :community/type 2]
     [:db/retract 1 :community/type 2]]
    [[:db/retract 1 :community/type 2]])
  (test-into-tx
    [[:db/retract 1 :community/type 2]
     [:db/retract 1 :community/type 2]
     [:db/add 1 :community/type 2]]
    []))

(deftest retract-entity []
  (testing "all tempids"
    (test-into-tx
      [[:db/add "-1" :foo "asdf"]
       [:db/add "-1" :foo "bar"]]
      [[:db/retractEntity "-1"]]
      [])

    (testing "remove parent entity"
      (test-into-tx
        [[:db/add "-1" :foo "asdf"]
         [:db/add "-1" :ref "-2"]
         [:db/add "-2" :bar "asdf"]]
        [[:db/retractEntity "-1"]]
        [[:db/add "-2" :bar "asdf"]]))

    (testing "remove parent component entity"
      (test-into-tx
        [[:db/add "-1" :foo "asdf"]
         [:db/add "-1" :component "-2"]
         [:db/add "-2" :bar "asdf"]]
        [[:db/retractEntity "-1"]]
        []))

    (testing "remove child entity"
      (test-into-tx
        [[:db/add "-1" :ref "-2"]
         [:db/add "-2" :bar "asdf"]]
        [[:db/retractEntity "-2"]]
        [])

      (test-into-tx
        [[:db/add "-1" :foo "asdf"]
         [:db/add "-1" :ref "-2"]
         [:db/add "-2" :bar "asdf"]]
        [[:db/retractEntity "-2"]]
        [[:db/add "-1" :foo "asdf"]]))

    (testing "remove child component entity"
      (test-into-tx
        [[:db/add "-1" :component "-2"]
         [:db/add "-2" :bar "asdf"]]
        [[:db/retractEntity "-2"]]
        [])

      (test-into-tx
        [[:db/add "-1" :foo "asdf"]
         [:db/add "-1" :component "-2"]
         [:db/add "-2" :bar "asdf"]]
        [[:db/retractEntity "-2"]]
        [[:db/add "-1" :foo "asdf"]])))

  (testing "mixed ids"
    (test-into-tx
      [[:db/add 1 :foo "asdf"]
       [:db/add 1 :foo "bar"]]
      [[:db/retractEntity 1]]
      [[:db/retractEntity 1]])

    (testing "remove parent entity"
      (test-into-tx
        [[:db/add 1 :foo "asdf"]
         [:db/add 1 :ref "-2"]
         [:db/add "-2" :bar "asdf"]]
        [[:db/retractEntity 1]]
        [[:db/retractEntity 1]
         [:db/add "-2" :bar "asdf"]])

      (test-into-tx
        [[:db/add "-1" :foo "asdf"]
         [:db/add "-1" :ref 2]
         [:db/add 2 :bar "asdf"]]
        [[:db/retractEntity "-1"]]
        [[:db/add 2 :bar "asdf"]]))

    (testing "remove parent component entity"
      (test-into-tx
        [[:db/add 1 :foo "asdf"]
         [:db/add 1 :component "-2"]
         [:db/add "-2" :bar "asdf"]]
        [[:db/retractEntity 1]]
        [[:db/retractEntity 1]])

      (test-into-tx
        [[:db/add "-1" :foo "asdf"]
         [:db/add "-1" :component 2]
         [:db/add 2 :bar "asdf"]]
        [[:db/retractEntity "-1"]]
        ; entity 2 already exists, do not touch it
        [[:db/add 2 :bar "asdf"]]))

    (testing "remove child entity"
      (test-into-tx
        [[:db/add 1 :ref "-2"]
         [:db/add "-2" :bar "asdf"]]
        [[:db/retractEntity "-2"]]
        [])

      (test-into-tx
        [[:db/add "-1" :ref 2]
         [:db/add 2 :bar "asdf"]]
        [[:db/retractEntity 2]]
        [[:db/retractEntity 2]])

      (test-into-tx
        [[:db/add 1 :foo "asdf"]
         [:db/add 1 :ref "-2"]
         [:db/add "-2" :bar "asdf"]]
        [[:db/retractEntity "-2"]]
        [[:db/add 1 :foo "asdf"]])

      (test-into-tx
        [[:db/add "-1" :foo "asdf"]
         [:db/add "-1" :ref 2]
         [:db/add 2 :bar "asdf"]]
        [[:db/retractEntity 2]]
        [[:db/add "-1" :foo "asdf"]
         [:db/retractEntity 2]]))

    (testing "remove child component entity"
      (test-into-tx
        [[:db/add 1 :component "-2"]
         [:db/add "-2" :bar "asdf"]]
        [[:db/retractEntity "-2"]]
        [])

      (test-into-tx
        [[:db/add "-1" :component 2]
         [:db/add 2 :bar "asdf"]]
        [[:db/retractEntity 2]]
        ; entity 2 already exists, do not touch it
        [[:db/retractEntity 2]])

      (test-into-tx
        [[:db/add 1 :foo "asdf"]
         [:db/add 1 :component "-2"]
         [:db/add "-2" :bar "asdf"]]
        [[:db/retractEntity "-2"]]
        [[:db/add 1 :foo "asdf"]])

      (test-into-tx
        [[:db/add "-1" :foo "asdf"]
         [:db/add "-1" :component 2]
         [:db/add 2 :bar "asdf"]]
        [[:db/retractEntity 2]]
        [[:db/add "-1" :foo "asdf"]
         [:db/retractEntity 2]]))

    (testing "orphaned statements"
      (test-into-tx
        [[:db/add "-1" :ref "-2"]
         [:db/add "-2" :component "-3"]
         [:db/add "-3" :foo "asdf"]]
        [[:db/retractEntity "-3"]]
        [])

      (test-into-tx
        [[:db/add "-1" :ref "-2"]
         [:db/add "-2" :ref "-3"]
         [:db/add "-3" :ref "-4"]
         [:db/add "-4" :foo "asdf"]]
        [[:db/retractEntity "-4"]]
        [])

      (test-into-tx
        [[:db/add "-1" :ref "-2"]
         [:db/add "-2" :ref "-3"]
         [:db/add "-3" :ref 4]
         [:db/add 4 :foo "asdf"]]
        [[:db/retractEntity 4]]
        [[:db/retractEntity 4]]))))

(deftest edit-1 []
  (let [attribute {:db/ident :one
                   :db/valueType {:db/ident :db.type/string}
                   :db/cardinality {:db/ident :db.cardinality/one}}]
    (is (= (edit-entity "-1" attribute "a" "b")
           [[:db/retract "-1" :one "a"]
            [:db/add "-1" :one "b"]]))
    (is (= (edit-entity "-1" attribute "a" nil)
           [[:db/retract "-1" :one "a"]]))
    (is (= (edit-entity "-1" attribute "a" "")
           [[:db/retract "-1" :one "a"]
            [:db/add "-1" :one ""]])))

  (let [attribute {:db/ident :many
                   :db/valueType {:db/ident :db.type/string}
                   :db/cardinality {:db/ident :db.cardinality/many}}]
    (is (= (edit-entity "-1" attribute #{"a" "b"} #{"y" "b"})
           [[:db/retract "-1" :many "a"]
            [:db/add "-1" :many "y"]]))
    (is (= (edit-entity "-1" attribute #{"a" "b"} nil)
           [[:db/retract "-1" :many "a"]
            [:db/retract "-1" :many "b"]]))
    (is (= (edit-entity "-1" attribute #{"a" "b"} #{})
           [[:db/retract "-1" :many "a"]
            [:db/retract "-1" :many "b"]]))))
