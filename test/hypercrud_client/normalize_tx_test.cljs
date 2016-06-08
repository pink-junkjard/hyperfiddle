(ns hypercrud-client.normalize-tx-test
  (:require-macros [cljs.test :refer [deftest is]])
  (:require [cljs.test]
            [hypercrud-client.tx-util :refer [normalize-tx]]
            [clojure.set :refer [difference]]))


(defn check-normalize-tx [in expected-out]
  (let [out (normalize-tx [] in)]
    (do (is (= (count expected-out) (count out)))
        (let [expected-out (set expected-out)
              out (set out)]
          (do (is (empty? (difference out expected-out)) "Unexpected datoms")
              (is (empty? (difference expected-out out)) "Missing datoms"))))))

(deftest no-op []
  (check-normalize-tx [] []))

(deftest add-one []
  (check-normalize-tx
    [[:db/add 1 :district/name "Southwest"]
     [:db/add 1 :district/region 2]]
    [[:db/add 1 :district/name "Southwest"]
     [:db/add 1 :district/region 2]]
    ))

(deftest add-one-override-prior-matching-attr []
  (check-normalize-tx
    [[:db/add 1 :district/region 2]
     [:db/add 1 :district/name "Southwest"]
     [:db/retract 1 :district/name "Southwest"]
     [:db/add 1 :district/name ""]]
    [[:db/add 1 :district/region 2]
     [:db/add 1 :district/name ""]]
    ))

(deftest retract-one-cancel-matching-add []
  (check-normalize-tx
    [[:db/add 1 :district/name "Southwest"]
     [:db/add 1 :district/region 2]
     [:db/retract 1 :district/region 2]]
    [[:db/add 1 :district/name "Southwest"]])
  (check-normalize-tx
    [[:db/add 1 :district/name "Southwest"]
     [:db/retract 1 :district/name "Southwest"]]
    [])
  (check-normalize-tx
    [[:db/retract 1 :district/name "Southwest"]
     [:db/add 1 :district/name "Southwest"]]
    []))

(deftest retract-one-remove-when-not-exists-preserve-retract []
  (check-normalize-tx
    [[:db/retract 1 :district/region 2]]
    [[:db/retract 1 :district/region 2]]))

(deftest add-many-add-to-set []
  (check-normalize-tx
    [[:db/add 1 :community/type 20]
     [:db/add 1 :community/type 21]]
    [[:db/add 1 :community/type 20]
     [:db/add 1 :community/type 21]]))

(deftest retract-many-cancel-matching-add []
  (check-normalize-tx
    [[:db/add 1 :community/type 20]
     [:db/add 1 :community/type 21]
     [:db/retract 1 :community/type 21]]
    [[:db/add 1 :community/type 20]])
  (check-normalize-tx
    [[:db/add 1 :community/type 20]
     [:db/retract 1 :community/type 20]]
    [])
  (check-normalize-tx
    [[:db/retract 1 :community/type 20]
     [:db/add 1 :community/type 20]]
    []))

(deftest retract-many-empty-entity-preserve-retract []
  (check-normalize-tx
    [[:db/retract 1 :community/type 20]]
    [[:db/retract 1 :community/type 20]]))

(deftest add-many-cancel-matching-retract []
  (check-normalize-tx
    [[:db/add 1 :community/type 20]
     [:db/retract 1 :community/type 21]
     [:db/add 1 :community/type 21]]
    [[:db/add 1 :community/type 20]]))

(deftest longer-test-one []
  (check-normalize-tx
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
  (check-normalize-tx
    [[:db/add 1 :community/type 2]
     [:db/add 1 :community/type 2]]
    [[:db/add 1 :community/type 2]])
  (check-normalize-tx
    [[:db/add 1 :community/type 2]
     [:db/add 1 :community/type 2]
     [:db/retract 1 :community/type 2]]
    [])
  (check-normalize-tx
    [[:db/retract 1 :community/type 2]
     [:db/retract 1 :community/type 2]]
    [[:db/retract 1 :community/type 2]])
  (check-normalize-tx
    [[:db/retract 1 :community/type 2]
     [:db/retract 1 :community/type 2]
     [:db/add 1 :community/type 2]]
    []))
