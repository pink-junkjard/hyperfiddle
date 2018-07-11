(ns hyperfiddle.security.domains-test
  (:require [clojure.test :refer [join-fixtures deftest is use-fixtures testing]]
            [datomic.api :as d]
            [hyperfiddle.integration-fixtures :as fixtures]
            [hyperfiddle.io.transact :as transact]
            [hyperfiddle.security :as security]
            [hyperfiddle.security.entity-ownership :as entity-ownership])
  (:import (java.util UUID)))


(def schema
  [{:db/ident :hyperfiddle/owners
    :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/many}

   {:db/ident :person/email
    :db/valueType :db.type/string
    :db/unique :db.unique/identity
    :db/cardinality :db.cardinality/one}

   {:db/ident :person/name
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}

   {:db/ident :person/age
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one}

   {:db/ident :person/friends
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/many}

   {:db/ident :fiddle/link
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/many
    :db/isComponent true}

   {:db/ident :link/rel
    :db/valueType :db.type/keyword
    :db/cardinality :db.cardinality/one}])

(def db-owner (UUID/randomUUID))

(use-fixtures :each
  (join-fixtures
    [(fixtures/domains-fixture #{db-owner})
     (fixtures/db-fixture fixtures/test-uri #{db-owner} db-owner
                          :schema schema
                          :security ::security/custom
                          :custom-write-sec (str `entity-ownership/write-domains))]))

(deftest test-merging-tx-statements []
  (let [email "asdf@example.com"]
    (let [tx [[:db/add "-1" :person/email email]
              [:db/add "-1" :person/name "Asdf"]]]
      (testing "appends owner to new entity"
        (is (= (conj tx [:db/add "-1" :hyperfiddle/owners db-owner])
               (transact/process-tx fixtures/test-domains-uri db-owner fixtures/test-uri tx))))
      (transact/transact! fixtures/test-domains-uri db-owner {fixtures/test-uri tx}))

    (let [mergable-tx [[:db/add "-1" :person/email email]
                       [:db/add "-1" :person/age 1]]]
      (testing "fails for non-owner"
        (is (thrown-with-msg?
              RuntimeException #"user tx failed validation"
              (transact/process-tx fixtures/test-domains-uri "someone-else" fixtures/test-uri mergable-tx))))

      (testing "no owner reappended to existing entity"
        (is (= mergable-tx (transact/process-tx fixtures/test-domains-uri db-owner fixtures/test-uri mergable-tx)))))))

(deftest test-merging-tx-maps []
  (let [email "asdf@example.com"]
    (let [tx [{:db/id "-1"
               :person/email email
               :person/name "Asdf"}]]
      (testing "appends owner to new entity"
        (is (= (conj tx [:db/add "-1" :hyperfiddle/owners db-owner])
               (transact/process-tx fixtures/test-domains-uri db-owner fixtures/test-uri tx))))
      (transact/transact! fixtures/test-domains-uri db-owner {fixtures/test-uri tx}))

    (let [mergable-tx [{:db/id "-1"
                        :person/email email
                        :person/age 1}]]
      (testing "fails for non-owner"
        (is (thrown-with-msg?
              RuntimeException #"user tx failed validation"
              (transact/process-tx fixtures/test-domains-uri "someone-else" fixtures/test-uri mergable-tx))))

      (testing "no owner reappended to existing entity"
        (is (= mergable-tx (transact/process-tx fixtures/test-domains-uri db-owner fixtures/test-uri mergable-tx)))))))

(deftest test-components-statements []
  (let [email "asdf@example.com"]
    (let [tx [[:db/add "-1" :fiddle/link "-2"]
              [:db/add "-1" :person/email email]
              [:db/add "-2" :link/rel :some-link]]]
      (testing "appends owner only to parent entity"
        (is (= (conj tx [:db/add "-1" :hyperfiddle/owners db-owner])
               (transact/process-tx fixtures/test-domains-uri db-owner fixtures/test-uri tx))))
      (transact/transact! fixtures/test-domains-uri db-owner {fixtures/test-uri tx}))

    (let [link-id (d/q '[:find ?e . :where [?e :link/rel :some-link]] (d/db (d/connect (str fixtures/test-uri))))]
      (testing "update component"
        (let [tx [[:db/add link-id :person/name "Asdf"]]]
          (testing "fails for non-owner"
            (is (thrown-with-msg?
                  RuntimeException #"user tx failed validation"
                  (transact/process-tx fixtures/test-domains-uri "someone-else" fixtures/test-uri tx))))

          (testing "succeeds for owner, and no additional statements added"
            (is (= tx (transact/process-tx fixtures/test-domains-uri db-owner fixtures/test-uri tx))))))

      (testing "multiple component"
        (let [tx [[:db/add link-id :fiddle/link "-1"]
                  [:db/add "-1" :person/name "Qwerty"]
                  [:db/add "-1" :fiddle/link "-2"]
                  [:db/add "-2" :person/name "3rd child"]]]
          (testing "fails for non-owner"
            (is (thrown-with-msg?
                  RuntimeException #"user tx failed validation"
                  (transact/process-tx fixtures/test-domains-uri "someone-else" fixtures/test-uri tx))))

          (testing "succeeds for owner, and no additional statements added"
            (is (= tx (transact/process-tx fixtures/test-domains-uri db-owner fixtures/test-uri tx))))))

      (testing "retract component"
        (let [tx [[:db/retractEntity link-id]]]
          (testing "fails for non-owner"
            (is (thrown-with-msg?
                  RuntimeException #"user tx failed validation"
                  (transact/process-tx fixtures/test-domains-uri "someone-else" fixtures/test-uri tx))))

          (testing "succeeds for owner, and no additional statements added"
            (is (= tx (transact/process-tx fixtures/test-domains-uri db-owner fixtures/test-uri tx)))))))))

(deftest test-components-maps []
  (let [email "asdf@example.com"]
    (let [tx [{:db/id "-1"
               :person/email email
               :fiddle/link {:db/id "-2"
                             :link/rel :some-link}}]]
      (testing "appends owner only to parent entity"
        (is (= (conj tx [:db/add "-1" :hyperfiddle/owners db-owner])
               (transact/process-tx fixtures/test-domains-uri db-owner fixtures/test-uri tx))))
      (transact/transact! fixtures/test-domains-uri db-owner {fixtures/test-uri tx}))

    (let [link-id (d/q '[:find ?e . :where [?e :link/rel :some-link]] (d/db (d/connect (str fixtures/test-uri))))]
      (testing "update component"
        (let [tx [{:db/id link-id :person/name "Asdf"}]]
          (testing "fails for non-owner"
            (is (thrown-with-msg?
                  RuntimeException #"user tx failed validation"
                  (transact/process-tx fixtures/test-domains-uri "someone-else" fixtures/test-uri tx))))

          (testing "succeeds for owner, and no additional statements added"
            (is (= tx (transact/process-tx fixtures/test-domains-uri db-owner fixtures/test-uri tx))))))

      (testing "multiple component"
        (let [tx [{:db/id link-id
                   :fiddle/link {:db/id "-1"
                                 :person/name "Qwerty"
                                 :fiddle/link {:db/id "-2"
                                               :person/name "3rd child"}}}]]
          (testing "fails for non-owner"
            (is (thrown-with-msg?
                  RuntimeException #"user tx failed validation"
                  (transact/process-tx fixtures/test-domains-uri "someone-else" fixtures/test-uri tx))))

          (testing "succeeds for owner, and no additional statements added"
            (is (= tx (transact/process-tx fixtures/test-domains-uri db-owner fixtures/test-uri tx))))))

      (testing "retract component"
        (let [tx [[:db/retractEntity link-id]]]
          (testing "fails for non-owner"
            (is (thrown-with-msg?
                  RuntimeException #"user tx failed validation"
                  (transact/process-tx fixtures/test-domains-uri "someone-else" fixtures/test-uri tx))))

          (testing "succeeds for owner, and no additional statements added"
            (is (= tx (transact/process-tx fixtures/test-domains-uri db-owner fixtures/test-uri tx)))))))))
