(ns hyperfiddle.ide.console-links-test
  (:require
    [clojure.core.match :refer [match]]
    [clojure.string :as string]
    [clojure.test :refer [deftest is]]
    [contrib.data :refer [ungroup transpose]]
    [contrib.ct :refer [unwrap]]
    [contrib.eval :as eval]
    [contrib.reactive :as r]
    [contrib.reader]
    [contrib.template :as template]
    [contrib.try$ :refer [try-either]]
    [contrib.uri :refer [->URI]]
    [datascript.parser :as parser #?@(:cljs [:refer [FindRel FindColl FindTuple FindScalar Variable Aggregate Pull]])]
    [fixtures.ctx :refer [ctx result-coll query-coll]]
    [hypercrud.browser.field :as field]
    [hyperfiddle.ide.console-links :refer [console-link console-links-e
                                           query-links normalize-result console-links-rules query-links-impl]])
  #?(:clj (:import (datascript.parser FindRel FindColl FindTuple FindScalar Variable Aggregate Pull))))


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

(def queries
  {FindColl '[:find [(pull ?e [:db/id                       ; self
                               :reg/email                   ; not a ref
                               :reg/age                     ; not a ref
                               {:reg/gender [:db/ident]     ;self at gender level
                                :reg/shirt-size [:db/ident]}]) ; self at gender level
                     ...]
              :where [?e :reg/email]]
   FindRel '[:find
             (sum ?age)
             (pull ?g [:db/ident])
             :where
             [?e :reg/email ?email]
             [?e :reg/age ?age]
             [(clojure.string/includes? ?email "b")]
             [?e :reg/gender ?g]]
   FindTuple nil
   FindScalar nil})

(defn parse-query [q] (->> (try-either (parser/parse-query q)) (unwrap (constantly nil))))

(def qparsed (into {} (map (juxt key (comp parse-query val)) queries)))

(deftest datascript-match-parser-
  []
  (is (= (let [FindColl FindColl
               FindScalar FindScalar
               FindRel FindRel
               FindTuple FindTuple]
           (match [(type (:qfind (qparsed FindColl)))]
             [FindRel] :rel
             [FindColl] :coll
             [FindScalar] :scalar
             [FindTuple] :tuple))
         :coll))
  )

(def results
  {FindColl [{:reg/email "alice"} {:reg/email "bob"}]
   FindRel [[20 {:db/ident :gender/male}] [65 {:db/ident :gender/female}]]
   FindTuple nil
   FindScalar nil})

(deftest normalize-result-
  []
  (is (= (normalize-result (:qfind (qparsed FindColl)) (results FindColl))
         [[{:reg/email "alice"}] [{:reg/email "bob"}]]))
  (is (= (normalize-result (:qfind (qparsed FindColl)) [])
         []))
  (is (= (normalize-result (:qfind (qparsed FindRel)) (results FindRel))
         (results FindRel)))
  )

(deftest console-link-internals
  []
  (is (= (console-links-e @(:hypercrud.browser/schemas ctx)
                          (:qfind (qparsed FindColl))
                          0
                          (first (parser/find-elements (:qfind (qparsed FindColl))))
                          (results FindColl))
         (query-links-impl @(:hypercrud.browser/schemas ctx)
                           (:qfind (qparsed FindColl))
                           (results FindColl))

         ; Check empty case, doesn't impact links with this result
         (query-links-impl @(:hypercrud.browser/schemas ctx)
                           (:qfind (qparsed FindColl))
                           [])
         (query-links @(:hypercrud.browser/schemas ctx)
                      (queries FindColl)
                      (results FindColl))
         '([[] #{:hf/new :hf/remove :hf/self}]
            [[:reg/gender] #{:hf/affix :hf/detach :hf/self}]
            [[:reg/shirt-size] #{:hf/affix :hf/detach :hf/self}])
         ))

  ;(contrib.data/transpose (normalize-result (:qfind (qparsed FindColl)) (results FindColl)))
  ;(normalize-result (:qfind (qparsed FindColl)) [])
  )

(def links '([[] #{:hf/new :hf/remove :hf/self}]
              [[:reg/gender] #{:hf/affix :hf/detach :hf/self}]
              [[:reg/shirt-size] #{:hf/affix :hf/detach :hf/self}]))

(deftest link-factory
  []
  (is (= (->> '([[:reg/gender] #{:hf/affix :hf/detach :hf/self}])
              (ungroup)
              (map console-link)
              (map (juxt :link/rel :link/path)))
         '([:hf/affix ":reg/gender"]
            [:hf/detach ":reg/gender"]
            [:hf/self ":reg/gender"])))
  )

(def matrix
  '[
    ["simple collection with links"
     [:find
      [(pull ?e [:db/id
                 :reg/email
                 {:reg/gender [:db/ident]}
                 {:reg/shirt-size [:db/ident]}])
       ...]
      :where [?e :reg/email]]
     [{:db/id 17592186046196, :reg/email "dustin@example.com", :reg/gender {:db/ident :gender/male}, :reg/shirt-size {:db/ident :shirt-size/mens-large}}
      {:db/id 17592186046763, :reg/email "bob@example.com", :reg/gender {:db/ident :gender/male}, :reg/shirt-size {:db/ident :shirt-size/mens-large}}]
     ([[] #{:hf/new :hf/remove :hf/self}]
       [[:reg/gender] #{:hf/affix :hf/detach :hf/self}]
       [[:reg/shirt-size] #{:hf/affix :hf/detach :hf/self}])]

    ["simple relation with one element"
     [:find
      (pull ?e [:db/id
                :reg/email
                {:reg/gender [:db/ident]}
                {:reg/shirt-size [:db/ident]}])
      :where [?e :reg/email]]
     [[{:db/id 17592186046196, :reg/email "dustin@example.com", :reg/gender {:db/ident :gender/male}, :reg/shirt-size {:db/ident :shirt-size/mens-large}}]
      [{:db/id 17592186046763, :reg/email "bob@example.com", :reg/gender {:db/ident :gender/male}, :reg/shirt-size {:db/ident :shirt-size/mens-large}}]]
     ([(0) #{:hf/remove :hf/self}]
       [(0 :reg/gender) #{:hf/affix :hf/detach :hf/self}]
       [(0 :reg/shirt-size) #{:hf/affix :hf/detach :hf/self}])]

    ["simple pull scalar with links (no hf/new on forms)"
     [:find
      (pull ?e [:db/id
                :reg/email
                {:reg/gender [:db/ident]}
                {:reg/shirt-size [:db/ident]}])
      .
      :where [?e :reg/email]]
     {:db/id 17592186046196, :reg/email "dustin@example.com", :reg/gender {:db/ident :gender/male}, :reg/shirt-size {:db/ident :shirt-size/mens-large}}
     ([[] #{:hf/remove :hf/self}]
       [[:reg/gender] #{:hf/affix :hf/detach :hf/self}]
       [[:reg/shirt-size] #{:hf/affix :hf/detach :hf/self}])
     ]


    ["simple relation with N elements"
     [:find
      (pull ?e [:db/id
                :reg/email
                {:reg/shirt-size [:db/ident]}])
      (pull ?g [:db/ident])
      :where
      [?e :reg/email ?email]
      [(clojure.string/includes? ?email "b")]
      [?e :reg/gender ?g]]
     [[{:db/id 17592186046763, :reg/email "bob@example.com", :reg/shirt-size {:db/ident :shirt-size/mens-large}} {:db/ident :gender/male}]
      [{:db/id 17592186046765, :reg/email "elizabeth@example.com", :reg/shirt-size {:db/ident :shirt-size/womens-medium}} {:db/ident :gender/female}]]
     ([(0) #{:hf/remove :hf/self}]
       [(0 :reg/shirt-size) #{:hf/affix :hf/detach :hf/self}]
       [(1) #{:hf/remove :hf/self}])
     ]

    ["relation with aggregate and pull alias"
     [:find
      (sum ?age)
      (pull ?g [:db/ident])
      (pull ?e [:db/id (:reg/gender :as :gender)])
      :where
      [?e :reg/email ?email]
      [?e :reg/age ?age]
      [(clojure.string/includes? ?email "b")]
      [?e :reg/gender ?g]]
     [[20 {:db/ident :gender/male} {:gender {:db/id 17592186046203}}] [65 {:db/ident :gender/female} {:gender {:db/id 17592186046204}}]]
     ([(1) #{:hf/remove :hf/self}]
       [(2) #{:hf/remove :hf/self}]
       [(2 :reg/gender) #{:hf/affix :hf/detach :hf/self}])
     ]

    ])

(deftest console-links-rules-
  []
  (for [[comment query result links] matrix]
    (is (= (query-links @(:hypercrud.browser/schemas ctx) query result)
           links)
        comment))
  )

(comment
  (is (= (transpose (normalize-result (:qfind (qparsed FindRel)) (results FindRel)))
         [[20 65] [#:db{:ident :gender/male} #:db{:ident :gender/female}]]))
  )