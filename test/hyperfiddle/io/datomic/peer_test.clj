(ns hyperfiddle.io.datomic.peer-test
  (:require
    [clojure.test :refer [deftest is]]
    [hyperfiddle.config]
    [hyperfiddle.domain]
    [hyperfiddle.io.datomic.core :as d]
    [hyperfiddle.io.datomic.peer]))


(def config {:domain {:databases
                      {"$" {:database/uri #uri "datomic:mem://app-db"}
                       #_#_"$hyperfiddle" {:database/uri #uri "datomic:mem://hyperfiddle"}
                       #_#_"$users" {:database/uri #uri "datomic:mem://hyperfiddle-users"}}}})

(def config (hyperfiddle.config/get-config config))
(def domain (hyperfiddle.config/get-domain config))

(defn with [$ tx]
  (:db-after (d/with $ {:tx-data tx})))

(declare $ q)

(deftest limit-1
  (def conn (hyperfiddle.domain/connect domain "$"))
  (def $ (-> (hyperfiddle.io.datomic.core/db conn)
           (with [{:db/ident :scratch/email :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
                  {:db/ident :scratch/gender :db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
                  {:db/ident :scratch/shirt-size :db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
                  {:db/ident :scratch/type :db/valueType :db.type/keyword :db/cardinality :db.cardinality/one}])
           (with [{:scratch/type :scratch/gender :db/ident :scratch/male}
                  {:scratch/type :scratch/gender :db/ident :scratch/female}])
           (with [{:scratch/type :scratch/shirt-size :db/ident :scratch/mens-small :scratch/gender :scratch/male}
                  {:scratch/type :scratch/shirt-size :db/ident :scratch/mens-medium :scratch/gender :scratch/male}
                  {:scratch/type :scratch/shirt-size :db/ident :scratch/mens-large :scratch/gender :scratch/male}
                  {:scratch/type :scratch/shirt-size :db/ident :scratch/womens-small :scratch/gender :scratch/female}
                  {:scratch/type :scratch/shirt-size :db/ident :scratch/womens-medium :scratch/gender :scratch/female}
                  {:scratch/type :scratch/shirt-size :db/ident :scratch/womens-large :scratch/gender :scratch/female}])
           (with [{:scratch/email "alice@example.com" :scratch/gender :scratch/female :scratch/shirt-size :scratch/womens-large}
                  {:scratch/email "bob@example.com" :scratch/gender :scratch/male :scratch/shirt-size :scratch/mens-large}
                  {:scratch/email "charlie@example.com" :scratch/gender :scratch/male :scratch/shirt-size :scratch/mens-medium}])))
  (def q (hyperfiddle.io.datomic.core/qf2 (hyperfiddle.domain/database domain "$")))

  (is (= [17592186045428 17592186045429 17592186045430]
        (q {:query '[:find [?e ...] :where [?e :scratch/email]] :args [$] :limit 50})
        (q {:query '[:find [?e ...] :where [?e :scratch/email]] :args [$] :limit -1})
        (q {:query '[:find [?e ...] :where [?e :scratch/email]] :args [$]})))
  )
