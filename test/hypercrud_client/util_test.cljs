(ns hypercrud-client.util-test
  (:require-macros [cljs.test :refer [deftest testing is]])
  (:require [cljs.test]
            [hypercrud-client.util :refer [normalize-tx]]))



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


(deftest test-simplify1 []

  (= (into #{} (normalize-tx
                 schema
                 [[:db/add 1 :district/name "Southwest"]
                  [:db/add 1 :district/region 2]]))
     #{[:db/add 1 :district/name "Southwest"]
       [:db/add 1 :district/region 2]}))


;(deftest test-simpliy2 []
;
;
;  (= (into #{} (normalize-tx
;                 schema
;                 [[:db/add 17592186045441 :district/name "Southwestaas"]
;                  [:db/add 17592186045436 :db/ident :region/nw3]
;                  [:db/add 17592186045441 :district/type "asdf"]
;                  [:db/add 17592186045436 :db/ident :region/nw33]
;                  [:db/add 17592186045436 :db/ident :region/nw333]
;                  [:db/add 17592186045441 :district/name "Southwestaasa"]
;                  [:db/add 17592186045441 :district/name "Southwestaasaa"]]))
;     #{[:db/add 17592186045441 :district/name "Southwestaasaa"]
;       [:db/add 17592186045436 :db/ident :region/nw333]})
;
;
;  (is (= true false)))