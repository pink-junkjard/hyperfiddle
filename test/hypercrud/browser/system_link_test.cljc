(ns hypercrud.browser.system-link-test
  (:require [clojure.string :as string]
            [clojure.test :refer [deftest is]]
            [contrib.eval :as eval]
            [contrib.reactive :as r]
            [contrib.template :as template]
            [contrib.uri :refer [->URI]]
            [hypercrud.browser.field :as field]
            [hypercrud.browser.auto-link :refer [txfn-remove]]
            [hypercrud.types.Entity :refer [->Entity]]))


; todo collapse these 3 into one test

; Fixing these needs proper context mocking
(deftest txfn-entity-remove []
  (let [f (eval/eval-string! txfn-remove)
        uri #uri "test"
        ctx {:cell-data (r/atom (->Entity uri {:db/id "entity"}))}]
    #_(is (= (f ctx nil nil)
           {:tx {uri [[:db.fn/retractEntity "entity"]]}}))))

(deftest txfn-value-remove-one []
  (let [f (eval/eval-string! txfn-remove)
        uri #uri "test"
        ctx {:value (r/atom (->Entity uri {:db/id "child"}))}]
    #_(is (= (f ctx nil nil)
           {:tx {uri [[:db.fn/retractEntity "child"]]}}))))

(deftest txfn-value-remove-many []
  (let [f (eval/eval-string! txfn-remove)
        uri #uri "test"
        ctx {:value (r/atom [(->Entity uri {:db/id "child 1"})
                             (->Entity uri {:db/id "child 2"})])}]
    #_(is (= (f ctx nil nil)
           {:tx {uri [[:db.fn/retractEntity "child 1"]
                      [:db.fn/retractEntity "child 2"]]}}))))

(deftest mt-fet-at
  []
  ; otherwise pointless, this test serves one important use: evaling and invoking affix.edn in the build
  (let [f (-> (template/load-resource "auto-txfn/affix.edn")
              string/trim
              eval/eval-string!)
       uri (->URI "test")
       ctx {:hypercrud.browser/domain {:domain/databases #{{:domain.database/name "$"
                                                            :domain.database/record {:database/uri uri}}}}
            :hypercrud.browser/field (r/atom {::field/source-symbol "$"})
            :hypercrud.browser/path [:parent/child]
            :hypercrud.browser/parent {:hypercrud.browser/data (r/atom (->Entity uri {:db/id "parent"}))}}
       modal-route [nil [{:db/id "child"}]]]
   (is (= (f ctx nil modal-route)
          {:tx {uri [[:db/add "parent" :parent/child "child"]]}}))))
