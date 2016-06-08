(ns hypercrud-client.util-test
  (:require-macros [cljs.test :refer [deftest testing is]])
  (:require [cljs.test]
            [hypercrud-client.util :refer [normalize-tx]]
            [clojure.set :refer [difference]]))



(def schema [
             ;; community
             {:db/ident :community/name
              :db/valueType :db.type/string
              :db/cardinality :db.cardinality/one}

             {:db/ident :community/url
              :db/valueType :db.type/string
              :db/cardinality :db.cardinality/one}

             {:db/ident :community/neighborhood
              :db/valueType :db.type/ref
              :db/cardinality :db.cardinality/one}

             {:db/ident :community/category
              :db/valueType :db.type/string
              :db/cardinality :db.cardinality/many}

             {:db/ident :community/orgtype
              :db/valueType :db.type/ref
              :db/cardinality :db.cardinality/one}

             {:db/ident :community/type
              :db/valueType :db.type/ref
              :db/cardinality :db.cardinality/many}

             ;; district
             {:db/ident :district/name
              :db/valueType :db.type/string
              :db/cardinality :db.cardinality/one
              :db/unique :db.unique/identity}

             {:db/ident :district/region
              :db/valueType :db.type/ref
              :db/cardinality :db.cardinality/one}])


(def cases
  {
   ;; :add :one
   [[:db/add 1 :district/name "Southwest"]
    [:db/add 1 :district/region 2]]
   [[:db/add 1 :district/name "Southwest"]
    [:db/add 1 :district/region 2]]

   ;; :add :one override prior value of this attr
   [[:db/add 1 :district/region 2]
    [:db/add 1 :district/name "Southwest"]
    [:db/add 1 :district/name ""]]
   [[:db/add 1 :district/region 2]
    [:db/add 1 :district/name ""]]

   ;; :retract :one - cancel matching :add
   [[:db/add 1 :district/name "Southwest"]
    [:db/add 1 :district/region 2]
    [:db/retract 1 :district/region 2]]
   [[:db/add 1 :district/name "Southwest"]]

   ;; :retract :one - remove when not exists = preserve :retract
   [[:db/retract 1 :district/region 2]]
   [[:db/retract 1 :district/region 2]]

   ;; :add :many - add to set
   [[:db/add 1 :community/type 20]
    [:db/add 1 :community/type 21]]
   [[:db/add 1 :community/type 20]
    [:db/add 1 :community/type 21]]

   ;; :retract :many - cancel matching :add
   [[:db/add 1 :community/type 20]
    [:db/add 1 :community/type 21]
    [:db/retract 1 :community/type 21]]
   [[:db/add 1 :community/type 20]]

   ;; :retract :many - remove from empty set = preserve :retract
   [[:retract 1 :community/type 20]]
   [[:retract 1 :community/type 20]]

   ;; :add :many - cancel matching :retract
   [[:add :community/type 20]
    [:retract :community/type 21]
    [:add :community/type 21]]
   [[:add :community/type 20]]

   })


(deftest test-simplify []
  (doall
    (map (fn [[in out]]
           (let [out' (normalize-tx schema in)]
             (do (is (= (count out) (count out')))
                 (let [out (set out)
                       out' (set out')]
                   (do (is (empty? (difference out' out)) "Unexpected datoms")
                       (is (empty? (difference out out')) "Missing datoms"))))))
         cases)))
