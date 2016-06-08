(ns hypercrud-client.tx-util-test
  (:require-macros [cljs.test :refer [deftest is]])
  (:require [cljs.test]
            [hypercrud-client.tx-util :refer [entity->datoms]]))

(deftest entity-datoms []
  (let [hc-data {:community/name "At Large in Ballard",
                 :community/url "http://blog.seattlepi.com/ballard/",
                 :community/neighborhood 17592186045456,
                 :community/category #{"news" "human interest"},
                 :community/orgtype 17592186045418,
                 :community/type #{17592186045424}}]
    (is (= (set (entity->datoms 1 hc-data))
           #{[:db/add 1 :community/neighborhood 17592186045456]
             [:db/add 1 :community/type 17592186045424]
             [:db/add 1 :community/orgtype 17592186045418]
             [:db/add 1 :community/name "At Large in Ballard"]
             [:db/add 1 :community/category "news"]
             [:db/add 1 :community/category "human interest"]
             [:db/add 1 :community/url "http://blog.seattlepi.com/ballard/"]}))))
