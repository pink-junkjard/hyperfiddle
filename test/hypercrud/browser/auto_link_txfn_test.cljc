(ns hypercrud.browser.auto-link-txfn-test
  (:require [clojure.string :as string]
            [clojure.test :refer [deftest is]]
            [contrib.eval :as eval]
            [contrib.reactive :as r]
            [contrib.template :as template]
            [contrib.uri :refer [->URI]]
            [hypercrud.types.Entity :refer [->Entity]]))


(deftest mt-fet-at []
  ; otherwise pointless, this test serves one important use: evaling and invoking mt-fet-at.edn in the build
  (let [f (-> (template/load-resource "auto-txfn/mt-fet-at.edn")
              string/trim
              eval/eval-string)
        uri (->URI "test")
        ctx {:uri uri
             :hypercrud.browser/attribute :parent/child
             :hypercrud.browser/parent {:hypercrud.browser/data (r/atom (->Entity uri {:db/id "parent"}))}}
        modal-route [nil [{:db/id "child"}]]]
    (is (= (f ctx nil modal-route)
           {:tx {uri [[:db/add "parent" :parent/child "child"]]}}))))
