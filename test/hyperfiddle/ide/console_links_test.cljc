(ns hyperfiddle.ide.console-links-test
  (:require
    [clojure.string :as string]
    [clojure.test :refer [deftest is]]
    [contrib.eval :as eval]
    [contrib.reactive :as r]
    [contrib.template :as template]
    [contrib.uri :refer [->URI]]
    [hypercrud.browser.field :as field]
    [hyperfiddle.fiddle :refer [txfn-remove]]))


; todo collapse these 3 into one test

; Fixing these needs proper context mocking
(deftest txfn-entity-remove []
  (let [f (eval/eval-string! txfn-remove)
        uri #uri "test"
        ctx {:hypercrud.browser/data (r/atom {:db/id "entity"})}]
    #_(is (= (f ctx nil nil)
           {:tx {uri [[:db.fn/retractEntity "entity"]]}}))))

(deftest txfn-value-remove-one []
  (let [f (eval/eval-string! txfn-remove)
        uri #uri "test"
        ctx {:hypercrud.browser/data (r/atom {:db/id "child"})}]
    #_(is (= (f ctx nil nil)
           {:tx {uri [[:db.fn/retractEntity "child"]]}}))))

(deftest txfn-value-remove-many []
  (let [f (eval/eval-string! txfn-remove)
        uri #uri "test"
        ctx {:hypercrud.browser/data (r/atom [{:db/id "child 1"}
                                              {:db/id "child 2"}])}]
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
            :hypercrud.browser/data (r/atom {:db/id "child"})
            :hypercrud.browser/parent {:hypercrud.browser/data (r/atom {:db/id "parent"})}}
       modal-route [nil [{:db/id "child"}]]]
   (is (= (f ctx nil modal-route)
          {:tx {uri [[:db/add "parent" :parent/child "child"]]}}))))