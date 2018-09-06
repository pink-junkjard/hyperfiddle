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

(deftest test-schema-changes []
  (testing "installing new attribute"
    (letfn [(f [tx]
              (testing "fails for non-owner"
                (is (thrown-with-msg?
                      RuntimeException #"user tx failed validation"
                      (transact/process-tx fixtures/test-domains-uri "someone-else" fixtures/test-uri tx))))

              (testing "succeeds for owner, and no additional statements added"
                (is (= tx (transact/process-tx fixtures/test-domains-uri db-owner fixtures/test-uri tx)))))]
      (f [{:db/id "-1"
           :db/ident :some-new-attr
           :db/valueType :db.type/string
           :db/cardinality :db.cardinality/one}])
      (f [[:db/add "-1" :db/ident :some-new-attr]
          [:db/add "-1" :db/valueType :db.type/string]
          [:db/add "-1" :db/cardinality :db.cardinality/one]])))

  (testing "altering existing attribute"
    (letfn [(f [tx]
              (testing "fails for non-owner"
                (is (thrown-with-msg?
                      RuntimeException #"user tx failed validation"
                      (transact/process-tx fixtures/test-domains-uri "someone-else" fixtures/test-uri tx))))

              (testing "succeeds for owner, and no additional statements added"
                (is (= tx (transact/process-tx fixtures/test-domains-uri db-owner fixtures/test-uri tx)))))]
      (f [{:db/id "-1"
           :db/ident :fiddle/link
           :db/doc "blah blah blah"}])
      (f [[:db/add :fiddle/link :db/doc "blah blah blah"]]))))

(deftest component-many []
  (let [person-a (UUID/randomUUID)
        person-b (UUID/randomUUID)]
    (transact/transact! fixtures/test-domains-uri person-a {fixtures/test-uri [{:person/name "person a"}]})
    (transact/transact! fixtures/test-domains-uri person-b {fixtures/test-uri [{:person/name "person b"}]})

    (let [db (d/db (d/connect (str fixtures/test-uri)))
          a (d/q '[:find ?e . :where [?e :person/name "person a"]] db)
          b (d/q '[:find ?e . :where [?e :person/name "person b"]] db)]
      (letfn [(f [tx]
                (testing "fails for person-a"
                  (is (thrown-with-msg?
                        RuntimeException #"user tx failed validation"
                        (transact/process-tx fixtures/test-domains-uri person-a fixtures/test-uri tx))))

                (testing "fails for person-b"
                  (is (thrown-with-msg?
                        RuntimeException #"user tx failed validation"
                        (transact/process-tx fixtures/test-domains-uri person-b fixtures/test-uri tx))))

                (testing "fails for owner"
                  (is (thrown-with-msg?
                        RuntimeException #"user tx failed validation"
                        (transact/process-tx fixtures/test-domains-uri db-owner fixtures/test-uri tx)))))]
        (testing "add link to person-a"
          (f [[:db/add a :fiddle/link b]])
          (f [{:db/id a :fiddle/link [b]}])
          (f [{:db/id a :fiddle/link [{:db/id b}]}]))

        (testing "add link to person-b"
          (f [[:db/add b :fiddle/link a]])
          (f [{:db/id b :fiddle/link [a]}])
          (f [{:db/id b :fiddle/link [{:db/id a}]}]))))))

(deftest non-component-refs []
  (testing "succeeds and adds owner to asdf and qwerty"
    (letfn [(f [tx]
              (is (= (into tx [[:db/add "-1" :hyperfiddle/owners db-owner]
                               [:db/add "-2" :hyperfiddle/owners db-owner]])
                     (transact/process-tx fixtures/test-domains-uri db-owner fixtures/test-uri tx))))]
      (f [{:db/id "-1"
           :person/name "asdf"
           :person/friends [{:db/id "-2"
                             :person/name "qwerty"}]}])
      (f [[:db/add "-1" :person/name "asdf"]
          [:db/add "-1" :person/friends "-2"]
          [:db/add "-2" :person/name "qwerty"]])))

  (let [person-a (UUID/randomUUID)
        person-b (UUID/randomUUID)]
    (transact/transact! fixtures/test-domains-uri person-a {fixtures/test-uri [{:person/name "person a"}]})
    (transact/transact! fixtures/test-domains-uri person-b {fixtures/test-uri [{:person/name "person b"}]})

    (let [db (d/db (d/connect (str fixtures/test-uri)))
          a (d/q '[:find ?e . :where [?e :person/name "person a"]] db)
          b (d/q '[:find ?e . :where [?e :person/name "person b"]] db)]
      (letfn [(f [tx]
                (testing "fails for person-a"
                  (is (thrown-with-msg?
                        RuntimeException #"user tx failed validation"
                        (transact/process-tx fixtures/test-domains-uri person-a fixtures/test-uri tx))))

                (testing "fails for person-b"
                  (is (thrown-with-msg?
                        RuntimeException #"user tx failed validation"
                        (transact/process-tx fixtures/test-domains-uri person-b fixtures/test-uri tx))))

                (testing "succeeds for db-owner"
                  (is (= tx (transact/process-tx fixtures/test-domains-uri db-owner fixtures/test-uri tx)))))]
        (testing "modifying child friend while adding to person-a"
          (f [{:db/id a :person/friends [{:db/id b :person/name "person a+"}]}])))

      (letfn [(f [tx]
                (testing "fails for person-b"
                  (is (thrown-with-msg?
                        RuntimeException #"user tx failed validation"
                        (transact/process-tx fixtures/test-domains-uri person-b fixtures/test-uri tx))))

                (testing "succeeds for person-a"
                  (is (= tx (transact/process-tx fixtures/test-domains-uri person-a fixtures/test-uri tx))))

                (testing "succeeds for owner"
                  (is (= tx (transact/process-tx fixtures/test-domains-uri db-owner fixtures/test-uri tx)))))]
        (testing "add friend to person-a"
          (f [[:db/add a :person/friends b]])
          (f [{:db/id a :person/friends [b]}])
          ;(f [{:db/id a :person/friends [{:db/id b}]}]) ;todo
          ))

      (letfn [(f [tx]
                (testing "fails for person-a"
                  (is (thrown-with-msg?
                        RuntimeException #"user tx failed validation"
                        (transact/process-tx fixtures/test-domains-uri person-a fixtures/test-uri tx))))

                (testing "succeeds for person-b"
                  (is (= tx (transact/process-tx fixtures/test-domains-uri person-b fixtures/test-uri tx))))

                (testing "succeeds for owner"
                  (is (= tx (transact/process-tx fixtures/test-domains-uri db-owner fixtures/test-uri tx)))))]
        (testing "add friend to person-b"
          (f [[:db/add b :person/friends a]])
          (f [{:db/id b :person/friends [a]}])
          ;(f [{:db/id b :person/friends [{:db/id a}]}]) ; todo
          )))))

(deftest sever-component-parent-child []
  (let [tx [{:person/name "parent"
             :fiddle/link [{:person/name "child"}]}]]
    (transact/transact! fixtures/test-domains-uri db-owner {fixtures/test-uri tx}))

  (let [parent-id (d/q '[:find ?e . :where [?e :person/name "parent"]] (d/db (d/connect (str fixtures/test-uri))))
        child-id (d/q '[:find ?e . :where [?e :person/name "child"]] (d/db (d/connect (str fixtures/test-uri))))
        tx [[:db/retract parent-id :fiddle/link child-id]]]
    (testing "fails for non-owner"
      (is (thrown-with-msg?
            RuntimeException #"user tx failed validation"
            (transact/process-tx fixtures/test-domains-uri "someone-else" fixtures/test-uri tx))))

    (testing "succeeds for owner, and owner is assigned to child"
      (is (= (conj tx [:db/add child-id :hyperfiddle/owners db-owner])
             (transact/process-tx fixtures/test-domains-uri db-owner fixtures/test-uri tx))))))

(deftest create-component-parent-child []
  (let [person-a (UUID/randomUUID)]
    (let [tx [{:person/name "parent"}
              {:person/name "child"
               :person/email "asdf"}]]
      (transact/transact! fixtures/test-domains-uri person-a {fixtures/test-uri tx}))

    (let [parent-id (d/q '[:find ?e . :where [?e :person/name "parent"]] (d/db (d/connect (str fixtures/test-uri))))
          child-id (d/q '[:find ?e . :where [?e :person/name "child"]] (d/db (d/connect (str fixtures/test-uri))))
          tx [[:db/add parent-id :fiddle/link child-id]]]
      (testing "fails for non-owner"
        (is (thrown-with-msg?
              RuntimeException #"user tx failed validation"
              (transact/process-tx fixtures/test-domains-uri "someone-else" fixtures/test-uri tx))))

      (testing "succeeds for owner, and owner is removed from child"
        (is (= (conj tx [:db/retract child-id :hyperfiddle/owners person-a])
               (transact/process-tx fixtures/test-domains-uri person-a fixtures/test-uri tx)))))))

(defn infinite-component-loop []
  ; todo test create and sever parent/child loops
  ; this creates an unhydratable entity in datomic, we need to fail fast and not stack overflow
  #_(let [tx [[:db/add "-1" :person/name "person1"]
              [:db/add "-2" :person/name "person2"]
              [:db/add "-1" :fiddle/link "-2"]
              [:db/add "-2" :fiddle/link "-1"]]]
      (testing "appends owner only to parent entity"
        (is (= (conj tx [:db/add "-1" :hyperfiddle/owners db-owner])
               (transact/process-tx fixtures/test-domains-uri db-owner fixtures/test-uri tx))))
      (transact/transact! fixtures/test-domains-uri db-owner {fixtures/test-uri tx})))

(deftest hybrid-relationships []
  (testing "parent-:db.part/db child-:db.part/user non-component"
    (let [tx [{:db/id "-1"
               :db/ident :attr
               :db/cardinality :db.cardinality/many
               :db/valueType :db.type/bigdec
               :person/friends [{:db/id "-2" :person/name "asdf"}]}]]
      (testing "succeeds and owner is added to child"
        (is (= (conj tx [:db/add "-2" :hyperfiddle/owners db-owner])
               (transact/process-tx fixtures/test-domains-uri db-owner fixtures/test-uri tx))))))

  #_(testing "parent-:db.part/db child-:db.part/user component"
      (let [tx [{:db/id "-1"
                 :db/ident :attr
                 :db/cardinality :db.cardinality/many
                 :db/valueType :db.type/bigdec
                 :fiddle/link [{:db/id "-2" :person/name "asdf"}]}]]
        (testing "succeeds and owner is added to child"
          (is (= (conj tx [:db/add "-2" :hyperfiddle/owners db-owner])
                 (transact/process-tx fixtures/test-domains-uri db-owner fixtures/test-uri tx))))))

  (testing "merging two parents by child component"
    (let [email "asdf@example.com"]
      (let [tx [[:db/add "-1" :person/name "parent a"]
                [:db/add "-2" :person/name "child"]
                [:db/add "-2" :person/email email]
                [:db/add "-1" :fiddle/link "-2"]]]
        (transact/transact! fixtures/test-domains-uri db-owner {fixtures/test-uri tx}))

      (let [tx [[:db/add "-1" :person/name "parent b"]
                [:db/add "-1" :fiddle/link [:person/email email]]]]
        (testing "fails for non-owner"
          (is (thrown-with-msg?
                RuntimeException #"user tx failed validation"
                (transact/process-tx fixtures/test-domains-uri "someone-else" fixtures/test-uri tx))))

        (testing "succeeds for owner, and owner added to parent b"
          (is (= (conj tx [:db/add "-1" :hyperfiddle/owners db-owner])
                 (transact/process-tx fixtures/test-domains-uri db-owner fixtures/test-uri tx))))))))

(deftest test-part-tx-changes []
  (let [email "asdf@example.com"]
    (let [tx [{:db/id "-1"
               :person/name "Asdf"
               :person/email email}
              [:db/add "datomic.tx" :db/doc "blah blah"]]]
      (testing "fails for non-owner"
        (is (thrown-with-msg?
              RuntimeException #"user tx failed validation"
              (transact/process-tx fixtures/test-domains-uri "someone-else" fixtures/test-uri tx))))
      (testing "succeeds for owner, and only hf/owner added to user partition"
        (is (= (conj tx [:db/add "-1" :hyperfiddle/owners db-owner])
               (transact/process-tx fixtures/test-domains-uri db-owner fixtures/test-uri tx)))))

    (let [$ (d/db (d/connect (str fixtures/test-uri)))
          some-tx-id (d/q '[:find ?tx . :in $ :where [?e :db/ident _ ?tx]] $)
          _ (assert (= (d/part some-tx-id) (d/entid $ :db.part/tx)))
          tx [[:db/add some-tx-id :db/doc "asdf"]]]
      (testing "fails for non-owner"
        (is (thrown-with-msg?
              RuntimeException #"user tx failed validation"
              (transact/process-tx fixtures/test-domains-uri "someone-else" fixtures/test-uri tx))))

      (testing "succeeds for owner, and no additional statements added"
        (is (= tx (transact/process-tx fixtures/test-domains-uri db-owner fixtures/test-uri tx)))))))

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

(deftest test-merging-components-statements []
  (let [email "asdf@example.com"]
    (let [tx [[:db/add "-1" :fiddle/link "-2"]
              [:db/add "-2" :link/rel :some-link]
              [:db/add "-2" :person/email email]]]
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

      (let [mergable-tx [[:db/add "-1" :person/email email]
                         [:db/add "-1" :person/age 1]]]
        (testing "fails for non-owner"
          (is (thrown-with-msg?
                RuntimeException #"user tx failed validation"
                (transact/process-tx fixtures/test-domains-uri "someone-else" fixtures/test-uri mergable-tx))))

        (testing "succeeds for owner, and no additional statements added"
          (is (= mergable-tx (transact/process-tx fixtures/test-domains-uri db-owner fixtures/test-uri mergable-tx)))))

      (testing "retract component by lookup"
        (let [tx [[:db/retractEntity [:person/email email]]]]
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
