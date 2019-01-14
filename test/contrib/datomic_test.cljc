(ns contrib.datomic-test
  (:require
    [clojure.test :refer [deftest is testing]]
    [contrib.datomic :refer [pull-shape tree-derivative pull-enclosure pull-level
                             pull-traverse pull-union normalize-result]]
    [contrib.ct]
    [contrib.try$]
    [fixtures.ctx :refer [schema result-coll]]
    [fixtures.domains]
    [datascript.parser #?@(:cljs [:refer [FindRel FindColl FindTuple FindScalar Variable Aggregate Pull]])])
  #?(:clj (:import (datascript.parser FindRel FindColl FindTuple FindScalar Variable Aggregate Pull))))


(def pull-pattern-2
  '[:a/a
    [:a/comp-one :as "comp-one"]
    :a/comp-many
    *
    (limit :a/v 1)
    {(default :a/s {}) [* :b/a :b/b {:c/a [*]} {:c/b [:d/a]}]
     [:a/t :as "T"] [*]
     (limit :a/u 2) [*]
     :a/x [*]}
    (default :a/z "a/z default")])

(def pull-pattern-1 '[:reg/email
                      :reg/age
                      *
                      {:reg/gender [:db/ident]
                       :reg/shirt-size [:db/ident
                                        *]}
                      :db/id])

(deftest pull-shape-
  []
  (is (= [:a/a :a/comp-one :a/comp-many :a/v #:a{:s [:b/a :b/b #:c{:a []} #:c{:b [:d/a]}], :t [], :u [], :x []} :a/z]
         (pull-shape pull-pattern-2)))
  (is (= [:reg/email
          :reg/age
          #:reg{:gender [:db/ident],
                :shirt-size [:db/ident]}
          :db/id]
         (pull-shape pull-pattern-1)))
  (is (= (pull-shape [:db/id
                      :hyperblog.post/sort-index1
                      :hyperblog.post/published
                      :hyperblog.post/hidden
                      :fiddle/ident
                      {:hyperblog.nav/children 1}
                      {:hyperblog.post/related 1}
                      :hyperblog.post/title])
         [:db/id
          :hyperblog.post/sort-index1
          :hyperblog.post/published
          :hyperblog.post/hidden
          :fiddle/ident
          :hyperblog.post/title])))

(def pulled-tree-1 {:db/id 17592186046765,
                    :hyperfiddle/owners [#uuid "acd054a8-4e36-4d6c-a9ec-95bdc47f0d39"],
                    :reg/email "elizabeth@example.com",
                    :reg/age 65,
                    :reg/gender {:db/ident :gender/female},
                    :reg/shirt-size
                    {:db/id 17592186046209,
                     :db/ident :shirt-size/womens-medium,
                     :hyperfiddle/owners [#uuid "acd054a8-4e36-4d6c-a9ec-95bdc47f0d39"],
                     :reg/gender {:db/id 17592186046204}}})

(def derivative-tests
  [["nested :many"
    fixtures.domains/schema
    {:db/id 17592186045942,
     :domain/home-route "",
     :domain/databases
     [{:db/id 17592186046525,
       :domain.database/name "$",
       :domain.database/record
       {:db/id 17592186046087,
        :database/uri
        #uri "datomic:free://datomic:4334/foo-ak"}}],}
    [:db/id :domain/home-route {:domain/databases [:db/id :domain.database/name {:domain.database/record [:db/id :database/uri]}]}]]
   ])

(deftest pulled-tree-derivative-2
  []
  (let [t derivative-tests]
    (doseq [[doc schema tree derivative] t]
      (is (= (tree-derivative schema tree)
             derivative)
          doc)))
  )

(deftest pulled-tree-derivative-1
  []
  (is (= [:db/id]
         (tree-derivative schema {:db/id 17592186046204})))

  (is (= [:db/id :db/ident :hyperfiddle/owners #:reg{:gender [:db/id]}]
         (tree-derivative schema {:db/id 17592186046209,
                                         :db/ident :shirt-size/womens-medium,
                                         :hyperfiddle/owners [#uuid "acd054a8-4e36-4d6c-a9ec-95bdc47f0d39"],
                                         :reg/gender {:db/id 17592186046204}})))

  (is (= [:db/id
          :hyperfiddle/owners
          :reg/email
          :reg/age
          #:reg{:gender [:db/ident]}
          #:reg{:shirt-size [:db/id :db/ident :hyperfiddle/owners #:reg{:gender [:db/id]}]}]
         (tree-derivative schema pulled-tree-1)))
  )

(deftest pull-union-
  []
  (is (= (pull-union [:db/id {:reg/gender [:db/id]}]
                     [:db/ident {:reg/gender [:db/ident]}])
         [:db/id :db/ident #:reg{:gender [:db/id :db/ident]}]))

  (is (= (pull-union {:reg/gender [:db/id]}
                     {:reg/gender [:db/ident]})
         #:reg{:gender [:db/id :db/ident]}))

  (is (= (pull-union [:db/id {:reg/gender [:db/id]}]
                     [:db/ident {:reg/gender [:db/ident]}])
         [:db/id :db/ident #:reg{:gender [:db/id :db/ident]}]))

  (is (= (pull-union []
                     [:db/id {:reg/gender [:db/id]}]
                     [:db/ident {:reg/gender [:db/ident]}])
         [:db/id :db/ident #:reg{:gender [:db/id :db/ident]}]))

  (is (= (pull-union [:dustingetz.reg/email
                      :dustingetz.reg/name
                      ; :dustingetz.reg/age
                      ; :dustingetz.reg/birthdate
                      {:dustingetz.reg/gender [:db/ident]}
                      {:dustingetz.reg/shirt-size [:db/ident]}
                      :db/id])
         [:dustingetz.reg/email :dustingetz.reg/name :db/id #:dustingetz.reg{:gender [:db/ident]} #:dustingetz.reg{:shirt-size [:db/ident]}]))

  (def pull-pattern-3 [:reg/email :reg/age #:reg{:gender [:db/ident], :shirt-size [:db/ident]} :db/id])
  (def result-derivatives [[:db/id
                            :hyperfiddle/owners
                            :reg/email
                            :reg/age
                            #:reg{:gender [:db/ident]}
                            #:reg{:shirt-size [:db/id :db/ident :hyperfiddle/owners #:reg{:gender [:db/id]}]}]
                           [:db/id
                            :hyperfiddle/owners
                            :reg/email
                            :reg/name
                            :reg/age
                            #:reg{:gender [:db/ident]}
                            #:reg{:shirt-size [:db/id :db/ident :hyperfiddle/owners #:reg{:gender [:db/id]}]}]])
  (is (= [:reg/email
          :reg/age
          :db/id
          :hyperfiddle/owners
          :reg/name
          #:reg{:gender [:db/ident], :shirt-size [:db/ident :db/id :hyperfiddle/owners #:reg{:gender [:db/id]}]}]
         (apply pull-union pull-pattern-3 result-derivatives)))
  )



(deftest enclosing-pull-shape-
  []
  (pull-enclosure schema [] [{:db/ident :foo} {:db/id 10 :db/ident :yo}])
  (pull-enclosure schema [:db/ident] [])
  (pull-enclosure schema [] [{:db/id 17592186046209,
                                    :db/ident :shirt-size/womens-medium,
                                    :hyperfiddle/owners [#uuid "acd054a8-4e36-4d6c-a9ec-95bdc47f0d39"],
                                    :reg/gender {:db/id 17592186046204}}
                                   {:db/id 17592186046209,
                                    :db/ident :shirt-size/womens-medium,
                                    :reg/gender {:db/id 17592186046204}}
                                   ])
  (pull-enclosure schema (pull-shape pull-pattern-1) result-coll)
  (pull-enclosure schema (pull-shape pull-pattern-1) [])
  )

(deftest form-traverse-
  []
  (is (= (pull-traverse [:db/ident])
         (pull-traverse [:db/id])
         (pull-traverse [:db/id :db/ident])
         '([])))

  (is (= (pull-traverse [:reg/gender])
         (pull-traverse [{:reg/gender [:db/ident]}])
         (pull-traverse [{:reg/gender [:db/id]}])
         (pull-traverse [{:reg/gender [:db/id :db/ident]}])
         (pull-traverse [{:reg/gender []}])
         '([:reg/gender])))



  (is (= (pull-traverse [:reg/gender
                         :db/id                             ; In place order
                         {:reg/shirt-size [:db/ident
                                           :reg/gender
                                           :db/ident
                                           :db/id]}
                         :db/id                             ; Ignored, use first
                         ])
         '([:reg/gender]
            []
            [:reg/shirt-size]
            [:reg/shirt-size :reg/gender])))
  )

(def queries
  {FindColl '[:find [(pull ?e [:db/id                       ; self
                               :reg/email                   ; not a ref
                               :reg/age                     ; not a ref
                               {:reg/gender [:db/ident]     ;self at gender level
                                :reg/shirt-size [:db/ident]}]) ; self at gender level
                     ...]
              :where [?e :reg/email]]
   FindRel '[:find (sum ?age) (pull ?g [:db/ident])]
   FindTuple '[:find [(pull ?e [:db/id :fiddle/ident]) (max ?tx) ?entrypoint]]
   FindScalar nil})

(defn parse-query [q] (->> (contrib.try$/try-either (datascript.parser/parse-query q)) (contrib.ct/unwrap (constantly nil))))

(def qparsed (into {} (map (juxt key (comp parse-query val)) queries)))

(def results
  {FindColl [{:reg/email "alice"} {:reg/email "bob"}]
   FindRel [[20 {:db/ident :gender/male}] [65 {:db/ident :gender/female}]]
   FindTuple [{:db/id 136, :fiddle/ident :dustingetz/games} 13194139536334 true]
   FindScalar nil})

(deftest normalize-result-
  []
  (is (= (normalize-result (:qfind (qparsed FindColl)) (results FindColl))
         [[{:reg/email "alice"}] [{:reg/email "bob"}]]))
  (is (= (normalize-result (:qfind (qparsed FindColl)) [])
         []))
  (is (= (normalize-result (:qfind (qparsed FindRel)) (results FindRel))
         (results FindRel)))
  (is (= (normalize-result (:qfind (qparsed FindTuple)) (results FindTuple))
         [[{:db/id 136, :fiddle/ident :dustingetz/games} 13194139536334 true]]))
  )

(deftest pull-strata-
  []
  (def a [:dustingetz.reg/email
          :dustingetz.reg/name
          ; :dustingetz.reg/age
          ; :dustingetz.reg/birthdate
          {:dustingetz.reg/gender [:db/ident]}
          {:dustingetz.reg/shirt-size [:db/ident]}
          :db/id])
  (testing "nested pulls"
    (is (= (contrib.datomic/pull-level a)
           '(:dustingetz.reg/email :dustingetz.reg/name :dustingetz.reg/gender :dustingetz.reg/shirt-size :db/id)))
    (is (= (contrib.datomic/pull-shape-refine :dustingetz.reg/gender a)
           '(:db/ident))))

  (testing "identity"
    (is (= (pull-level [:db/id :db/ident :hyperfiddle/owners #:reg{:gender [:db/id]}])
           '(:db/id :db/ident :hyperfiddle/owners :reg/gender))))
  )

