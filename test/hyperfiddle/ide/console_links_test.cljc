(ns hyperfiddle.ide.console-links-test
  (:require
    [clojure.string :as string]
    [clojure.test :refer [deftest is]]
    [contrib.ct :refer [unwrap]]
    [contrib.eval :as eval]
    [contrib.reactive :as r]
    [contrib.reader]
    [contrib.template :as template]
    [contrib.uri :refer [->URI]]
    [fixtures.ctx :refer [ctx result-coll query-coll]]
    [hypercrud.browser.field :as field]
    [hyperfiddle.fiddle :refer [txfn-remove]]
    [hyperfiddle.ide.console-links :refer [query-links]]))


; todo collapse these 3 into one test

; Fixing these needs proper context mocking
(deftest txfn-entity-remove []
  (let [f (eval/eval-expr-str! txfn-remove)
        uri #uri "test"
        ctx {:hypercrud.browser/data (r/atom {:db/id "entity"})}]
    #_(is (= (f ctx nil nil)
           {:tx {uri [[:db.fn/retractEntity "entity"]]}}))))

(deftest txfn-value-remove-one []
  (let [f (eval/eval-expr-str! txfn-remove)
        uri #uri "test"
        ctx {:hypercrud.browser/data (r/atom {:db/id "child"})}]
    #_(is (= (f ctx nil nil)
           {:tx {uri [[:db.fn/retractEntity "child"]]}}))))

(deftest txfn-value-remove-many []
  (let [f (eval/eval-expr-str! txfn-remove)
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
              eval/eval-expr-str!)
       uri #uri "test"
       ctx {:hypercrud.browser/domain {:domain/databases #{{:domain.database/name "$"
                                                            :domain.database/record {:database/uri uri}}}}
            :hypercrud.browser/field (r/atom {::field/source-symbol "$"})
            :hypercrud.browser/path [:parent/child]
            :hypercrud.browser/data (r/atom {:db/id "child"})
            :hypercrud.browser/parent {:hypercrud.browser/data (r/atom {:db/id "parent"})}}
       modal-route [nil [{:db/id "child"}]]]
   (is (= (f ctx nil modal-route)
          {:tx {uri [[:db/add "parent" :parent/child "child"]]}}))))

(deftest foo-1
  []
  (is (= #{[:hf/self nil]
           [:hf/remove nil]
           [:hf/new nil]
           [:hf/self ":reg/gender"]
           [:hf/affix ":reg/gender"]
           [:hf/detach ":reg/gender"]
           [:hf/self ":reg/shirt-size"]
           [:hf/affix ":reg/shirt-size"]
           [:hf/detach ":reg/shirt-size"]}
         (->> (query-links @(:hypercrud.browser/schemas ctx)
                           []
                           '[:find [(pull ?e [:db/id        ; self
                                              :reg/email    ; not a ref
                                              :reg/age      ; not a ref
                                              {:reg/gender [:db/ident] ;self at gender level
                                               :reg/shirt-size [:db/ident]}]) ; self at gender level
                                    ...]
                             :where [?e :reg/email]])
              (map (juxt :link/rel :link/path))
              set)
         ))

  (is (= #{[:hf/self nil]
           [:hf/remove nil]
           [:hf/new nil]}
         (->> (query-links @(:hypercrud.browser/schemas ctx)
                           []
                           '[:find [(pull ?e [:db/id]) ...] :where [?e :reg/email]])
              (map (juxt :link/rel :link/path))
              set)
         ))
  )
