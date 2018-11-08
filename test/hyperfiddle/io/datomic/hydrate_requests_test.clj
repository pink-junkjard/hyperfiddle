(ns hyperfiddle.io.datomic.hydrate-requests-test
  (:require
    [cats.monad.exception :as exception]
    [clojure.test :refer [deftest is use-fixtures]]
    [datomic.api :as d]
    [hyperfiddle.io.datomic.hydrate-requests :as datomic-hydrate-requests]))


(def test-uri "datomic:mem://test")

(def schema [{:db/ident :person/name
              :db/valueType :db.type/string
              :db/cardinality :db.cardinality/one}
             {:db/ident :person/age
              :db/valueType :db.type/long
              :db/cardinality :db.cardinality/one}])

(use-fixtures :each (fn [f]
                      (d/create-database test-uri)
                      (d/transact (d/connect test-uri) schema)
                      (f)
                      (d/delete-database test-uri)))

(defn build-get-secure-db-with [& args]
  (let [get-secure-db-with+ (apply datomic-hydrate-requests/build-get-secure-db-with+ args)]
    (fn [& args] (exception/extract (apply get-secure-db-with+ args)))))

(deftest schema-alteration []
  (let [conn (d/connect test-uri)
        staged-branches [{:branch-ident nil
                          :uri test-uri
                          :tx [[:db/add "-1" :db/ident :x/y]
                               [:db/add "-1" :db/valueType :db.type/string]
                               [:db/add "-1" :db/cardinality :db.cardinality/one]]}]
        local-basis {test-uri (d/basis-t (d/db conn))}
        get-secure-db-with (build-get-secure-db-with staged-branches (atom {}) local-basis)
        $ (:db (get-secure-db-with test-uri "nil"))]
    (is (= (as-> (d/touch (d/entity $ :x/y)) entity
             (into {} entity)
             (dissoc entity :db/id))
           {:db/ident :x/y
            :db/valueType :db.type/string
            :db/cardinality :db.cardinality/one}))))

(deftest branch-once []
  (let [conn (d/connect test-uri)
        staged-branches [{:branch-ident nil
                          :uri test-uri
                          :tx [{:db/id "-1" :person/name "John" :person/age 30}]}]
        local-basis {test-uri (d/basis-t (d/db conn))}
        get-secure-db-with (build-get-secure-db-with staged-branches (atom {}) local-basis)
        $ (:db (get-secure-db-with test-uri "nil"))]
    (is (= (d/q '[:find ?age . :where [?person :person/age ?age] [?person :person/name "John"]] $) 30))))

(deftest branch-once-stale []
  (let [conn (d/connect test-uri)
        staged-branches [{:branch-ident nil
                          :uri test-uri
                          :tx [{:db/id "-1" :person/name "John" :person/age 30}]}]
        local-basis {test-uri (d/basis-t (d/db conn))}      ; get a stale basis before another user transacts
        _ @(d/transact conn [{:db/id "-1" :person/name "Bob" :person/age 50}])
        get-secure-db-with (build-get-secure-db-with staged-branches (atom {}) local-basis)]
    (is (thrown-with-msg? RuntimeException (re-pattern datomic-hydrate-requests/ERROR-BRANCH-PAST)
                          (get-secure-db-with test-uri "nil")))))

(deftest branch-popover []
  (let [conn (d/connect test-uri)
        staged-branches [{:branch-ident nil
                          :uri test-uri
                          :tx nil}
                         {:branch-ident "2"
                          :uri test-uri
                          :tx [{:db/id "-1" :person/name "John" :person/age 30}]}]
        local-basis {test-uri (d/basis-t (d/db conn))}
        get-secure-db-with (build-get-secure-db-with staged-branches (atom {}) local-basis)
        $-nil (:db (get-secure-db-with test-uri "nil"))
        $-2 (:db (get-secure-db-with test-uri "2"))]
    (is (= (d/q '[:find ?age . :where [?person :person/age ?age] [?person :person/name "John"]] $-nil) nil))
    (is (= (d/q '[:find ?age . :where [?person :person/age ?age] [?person :person/name "John"]] $-2) 30))))

(deftest branch-popover-stale []
  (let [conn (d/connect test-uri)
        staged-branches [{:branch-ident nil
                          :uri test-uri
                          :tx nil}
                         {:branch-ident "2"
                          :uri test-uri
                          :tx [{:db/id "-1" :person/name "John" :person/age 30}]}]
        local-basis {test-uri (d/basis-t (d/db conn))}      ; get a stale basis before another user transacts
        _ @(d/transact conn [{:db/id "-1" :person/name "Bob" :person/age 50}])
        get-secure-db-with (build-get-secure-db-with staged-branches (atom {}) local-basis)
        $-nil (:db (get-secure-db-with test-uri "nil"))]
    (is (= (d/q '[:find ?age . :where [?person :person/age ?age] [?person :person/name "John"]] $-nil) nil))
    (is (thrown-with-msg? RuntimeException (re-pattern datomic-hydrate-requests/ERROR-BRANCH-PAST)
                          (get-secure-db-with test-uri "2")))))

(deftest branch-twice []
  (let [conn (d/connect test-uri)
        staged-branches [{:branch-ident nil
                          :uri test-uri
                          :tx [{:db/id "-1" :person/name "John" :person/age 30}]}
                         {:branch-ident "2"
                          :uri test-uri
                          :tx [{:db/id "-2" :person/name "Alice" :person/age 40}]}]
        local-basis {test-uri (d/basis-t (d/db conn))}
        get-secure-db-with (build-get-secure-db-with staged-branches (atom {}) local-basis)
        $-nil (:db (get-secure-db-with test-uri "nil"))
        $-2 (:db (get-secure-db-with test-uri "2"))]
    (is (= (d/q '[:find ?age . :where [?person :person/age ?age] [?person :person/name "John"]] $-nil) 30))
    (is (= (d/q '[:find ?age . :where [?person :person/age ?age] [?person :person/name "Alice"]] $-nil) nil))
    (is (= (d/q '[:find ?age . :where [?person :person/age ?age] [?person :person/name "John"]] $-2) 30))
    (is (= (d/q '[:find ?age . :where [?person :person/age ?age] [?person :person/name "Alice"]] $-2) 40))))

(deftest branch-twice-stale []
  (let [conn (d/connect test-uri)
        staged-branches [{:branch-ident nil
                          :uri test-uri
                          :tx [{:db/id "-1" :person/name "John" :person/age 30}]}
                         {:branch-ident "2"
                          :uri test-uri
                          :tx [{:db/id "-2" :person/name "Alice" :person/age 40}]}]
        local-basis {test-uri (d/basis-t (d/db conn))}      ; get a stale basis before another user transacts
        _ @(d/transact conn [{:db/id "-1" :person/name "Bob" :person/age 50}])
        get-secure-db-with (build-get-secure-db-with staged-branches (atom {}) local-basis)]
    (is (thrown-with-msg? RuntimeException (re-pattern datomic-hydrate-requests/ERROR-BRANCH-PAST)
                          (get-secure-db-with test-uri "nil")))
    (is (thrown-with-msg? RuntimeException (re-pattern datomic-hydrate-requests/ERROR-BRANCH-PAST)
                          (get-secure-db-with test-uri "2")))))
