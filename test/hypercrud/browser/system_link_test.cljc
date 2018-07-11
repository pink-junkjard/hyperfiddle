(ns hypercrud.browser.system-link-test
  (:require [clojure.test :refer [deftest is]]
            [contrib.eval :as eval]
            [contrib.reactive :as r]
            [hypercrud.browser.system-link :refer [retract-formula]]
            [hypercrud.types.Entity :refer [->Entity]]))


; todo collapse these 3 into one test

; Fixing these needs proper context mocking
(deftest txfn-entity-remove []
  (let [f (eval/eval-string retract-formula)
        uri #uri "test"
        ctx {:uri uri
             :cell-data (r/atom (->Entity uri {:db/id "entity"}))}]
    #_(is (= (f ctx nil nil)
           {:tx {uri [[:db.fn/retractEntity "entity"]]}}))))

(deftest txfn-value-remove-one []
  (let [f (eval/eval-string retract-formula)
        uri #uri "test"
        ctx {:uri uri
             :value (r/atom (->Entity uri {:db/id "child"}))}]
    #_(is (= (f ctx nil nil)
           {:tx {uri [[:db.fn/retractEntity "child"]]}}))))

(deftest txfn-value-remove-many []
  (let [f (eval/eval-string retract-formula)
        uri #uri "test"
        ctx {:uri uri
             :value (r/atom [(->Entity uri {:db/id "child 1"})
                             (->Entity uri {:db/id "child 2"})])}]
    #_(is (= (f ctx nil nil)
           {:tx {uri [[:db.fn/retractEntity "child 1"]
                      [:db.fn/retractEntity "child 2"]]}}))))
