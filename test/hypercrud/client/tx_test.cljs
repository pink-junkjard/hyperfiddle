(ns hypercrud.client.tx-test
  (:require-macros [cljs.test :refer [deftest is]])
  (:require [cljs.test]
            [hypercrud.client.test-util :refer [check-tx]]
            [hypercrud.client.tx :refer [entity->statements pulled-tree-to-statements]]))



(def schema
  [#_{:db/ident :db/id :db/cardinality :db.cardinality/one}
   #_{:db/ident :db/ident :db/cardinality :db.cardinality/one}
   {:db/ident :community/name :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
   {:db/ident :community/url :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
   {:db/ident :community/neighborhood :db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
   {:db/ident :community/category :db/valueType :db.type/string :db/cardinality :db.cardinality/many}
   {:db/ident :community/orgtype :db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
   {:db/ident :community/type :db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
   {:db/ident :community/type2 :db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
   {:db/ident :community-type2/name :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
   {:db/ident :neighborhood/name :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
   {:db/ident :neighborhood/district :db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
   {:db/ident :district/name :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
   {:db/ident :district/region :db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
   ])

(defn group-by-assume-unique [f xs]
  (into {} (map (fn [x] [(f x) x]) xs)))

(def indexed-schema (group-by-assume-unique :db/ident schema))


(deftest entity->statements-test []
  (let [hc-data {:db/id 1
                 :community/name "At Large in Ballard",
                 :community/url "http://blog.seattlepi.com/ballard/",
                 :community/neighborhood {:db/id 17592186045456},
                 :community/category #{"news" "human interest"},
                 :community/orgtype {:db/id 17592186045418},
                 :community/type #{{:db/id 17592186045424}}}]
    (is (= (set (entity->statements indexed-schema hc-data))
           #{[:db/add 1 :community/neighborhood 17592186045456]
             [:db/add 1 :community/type 17592186045424]
             [:db/add 1 :community/orgtype 17592186045418]
             [:db/add 1 :community/name "At Large in Ballard"]
             [:db/add 1 :community/category "news"]
             [:db/add 1 :community/category "human interest"]
             [:db/add 1 :community/url "http://blog.seattlepi.com/ballard/"]}))))


(def pulled-tree
  {:db/id 1
   :community/name "dcomm"
   :community/type2 #{{:db/id 4 :community/type2 #{{:db/id 6
                                                    :community-type2/name "asdf"}}}
                      {:db/id 5 :community/type2 #{{:db/id 7
                                                    :community-type2/name "qwerty"}}}}
   :community/neighborhood {:db/id 2
                            :neighborhood/name "neighborhood"
                            :neighborhood/district {:db/id 3
                                                    :district/name "The District"}}})


(deftest pulled-tree-to-statements-test []
  (check-tx
    (pulled-tree-to-statements indexed-schema pulled-tree)
    [[:db/add 1 :community/name "dcomm"]
     [:db/add 1 :community/type2 4]
     [:db/add 4 :community/type2 6]
     [:db/add 6 :community-type2/name "asdf"]
     [:db/add 1 :community/type2 5]
     [:db/add 5 :community/type2 7]
     [:db/add 7 :community-type2/name "qwerty"]
     [:db/add 1 :community/neighborhood 2]
     [:db/add 2 :neighborhood/name "neighborhood"]
     [:db/add 2 :neighborhood/district 3]
     [:db/add 3 :district/name "The District"]]))
