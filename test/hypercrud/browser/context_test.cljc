(ns hypercrud.browser.context-test
  (:require
    [clojure.test :refer [deftest is testing]]
    [contrib.reactive :as r]
    [fixtures.race]
    [hypercrud.browser.context :as context]))


(def ctx (context/fiddle
           {:hypercrud.browser/route nil}
           (r/pure fixtures.race/fiddle)
           (r/pure fixtures.race/schemas)
           (r/pure fixtures.race/result)))

(deftest context
  []

  (testing "result indexing"
    (testing "FindColl rows are indexed by db/id"
      (is (= @(:hypercrud.browser/result-index ctx)
             {17592186046196 {:db/id 17592186046196,
                              :dustingetz.reg/email "dustin@example.com",
                              :dustingetz.reg/name "Dustin Getz",
                              :dustingetz.reg/gender #:db{:ident :dustingetz.gender/male},
                              :dustingetz.reg/shirt-size #:db{:ident :dustingetz.shirt-size/mens-large}},
              17592186046763 {:db/id 17592186046763,
                              :dustingetz.reg/email "bob@example.com",
                              :dustingetz.reg/gender #:db{:ident :dustingetz.gender/male},
                              :dustingetz.reg/shirt-size #:db{:ident :dustingetz.shirt-size/mens-large}}}))))

  (testing "fiddle level context is set"
    (is (= @(r/fmap :fiddle/ident (:hypercrud.browser/fiddle ctx))
           :tutorial.race/submission))
    (is (= @(:hypercrud.browser/eav ctx)
           [nil :tutorial.race/submission nil]))
    ; result-enclosure, schemas, qfind, link-index, validation

    (is (= @(:hypercrud.browser/result-enclosure ctx)
           [[:dustingetz.reg/email
             :dustingetz.reg/name                           ; name is absent from second row
             :db/id
             #:dustingetz.reg{:gender [:db/ident],
                              :shirt-size [:db/ident]}]])))

  (testing "refocus to self is noop and doesn't crash"
    (is (= (context/refocus ctx :tutorial.race/submission)
           ctx)))

  (testing "refocus to dependent didn't crash"
    (is (= @(:hypercrud.browser/eav (context/refocus ctx :dustingetz.reg/gender))
           [nil :dustingetz.reg/gender nil])))

  (testing "simple spreads"
    (is (= (count (for [[_ ctx] (context/spread-result ctx)
                        [_ ctx] (context/spread-rows ctx)
                        [i ctx] (context/spread-elements ctx)
                        [a ctx] (context/spread-attributes ctx)]
                    [i a])
                  )
           (* (count fixtures.race/result)
              (count (keys (first fixtures.race/result)))))))

  (testing "don't need rows to spread attrs"
    (is (= (for [[_ ctx] (context/spread-result ctx)
                 ; skip rows
                 [i ctx] (context/spread-elements ctx)
                 [a ctx] (context/spread-attributes ctx)]
             [a])
           '([:dustingetz.reg/email]
              [:dustingetz.reg/name]
              [:db/id]
              [:dustingetz.reg/gender]
              [:dustingetz.reg/shirt-size]))))

  (testing "infer element when find-element dimension=1"
    (is (= (for [[_ ctx] (context/spread-result ctx)
                 ; skip rows
                 ; skip elements
                 [a ctx] (context/spread-attributes ctx)]
             [a])
           '([:dustingetz.reg/email] [:dustingetz.reg/name] [:db/id]
              [:dustingetz.reg/gender] [:dustingetz.reg/shirt-size]))))


  (testing "depth"
    (is (= (for [[_ ctx] (context/spread-result ctx)
                 [_ ctx] (context/spread-rows ctx)]
             (context/depth ctx))
           '(1 1))))

  (testing "refocus"
    (is (= (for [[_ ctx] (context/spread-result ctx)
                 [_ ctx] (context/spread-rows ctx)]
             @(:hypercrud.browser/eav (context/refocus ctx :tutorial.race/submission)))
           (for [[_ ctx] (context/spread-result ctx)
                 [_ ctx] (context/spread-rows ctx)
                 [_ ctx] (context/spread-elements ctx)]
             @(:hypercrud.browser/eav (context/refocus ctx :tutorial.race/submission)))
           '([nil :tutorial.race/submission nil]
              [nil :tutorial.race/submission nil])))

    (is (= (for [[_ ctx] (context/spread-result ctx)
                 [_ ctx] (context/spread-rows ctx)]
             @(:hypercrud.browser/eav
                (context/refocus ctx :dustingetz.reg/gender)))
           '([17592186046196 :dustingetz.reg/gender :dustingetz.gender/male]
              [17592186046763 :dustingetz.reg/gender :dustingetz.gender/male])))
    )

  (testing "row"
    (testing "row data by dbid"
      #_(:hypercrud.browser/result-path (context/row ctx 17592186046196))
      #_(:hypercrud.browser/result (context/row ctx 17592186046196))
      ; not sure if element can be inferred in data. Row does not infer.
      (is (= @(context/data (-> ctx (context/element 0) (context/row 17592186046196)))
             {:db/id 17592186046196,
              :dustingetz.reg/email "dustin@example.com",
              :dustingetz.reg/name "Dustin Getz",
              :dustingetz.reg/gender #:db{:ident :dustingetz.gender/male},
              :dustingetz.reg/shirt-size #:db{:ident :dustingetz.shirt-size/mens-large}}))
      )

    (testing "does not set e"
      (is (= @(:hypercrud.browser/eav (context/row ctx 17592186046196))
             [nil :tutorial.race/submission nil]))

      )
    (testing "refocus from root to gender"
      (is (= (for [[_ ctx] (context/spread-rows ctx)
                   [_ ctx] (context/spread-elements ctx)]
               #_@(:hypercrud.browser/result-index ctx)
               #_@(:hypercrud.browser/result ctx)
               @(:hypercrud.browser/result-index (context/refocus-in-element ctx :dustingetz.reg/gender))
               )
             {17592186046196 {:db/id 17592186046196,
                              :dustingetz.reg/email "dustin@example.com",
                              :dustingetz.reg/name "Dustin Getz",
                              :dustingetz.reg/gender #:db{:ident :dustingetz.gender/male},
                              :dustingetz.reg/shirt-size #:db{:ident :dustingetz.shirt-size/mens-large}},
              17592186046763 {:db/id 17592186046763,
                              :dustingetz.reg/email "bob@example.com",
                              :dustingetz.reg/gender #:db{:ident :dustingetz.gender/male},
                              :dustingetz.reg/shirt-size #:db{:ident :dustingetz.shirt-size/mens-large}}}))

      (is (= (for [[_ ctx] (context/spread-rows ctx)
                   [_ ctx] (context/spread-elements ctx)]
               @(:hypercrud.browser/eav (context/refocus-in-element ctx :dustingetz.reg/gender)))
             '([17592186046196 :dustingetz.reg/gender :dustingetz.gender/male]
               [17592186046763 :dustingetz.reg/gender :dustingetz.gender/male])))
      )

    (testing "refocus from myself when already here but not at root")

    )
  )

#_(deftest deps-satisfied-1
  []
  (is (= (deps-satisfied? [0 :fiddle/links] [0 :fiddle/links :link/fiddle]) false))
  (is (= (deps-satisfied? [0 :gender] [0 :shirt-size]) true))
  (is (= (deps-satisfied? [0] [0 :shirt-size]) true))
  (is (= (deps-satisfied? [0] [0 :fiddle/links :link/fiddle]) false))
  (is (= (deps-satisfied? [0 :fiddle/links] [0 :fiddle/links :link/fiddle]) false))
  (is (= (deps-satisfied? [] [0]) false))
  (is (= (deps-satisfied? [] [0 :user/user-id]) false)) ; This becomes true if a relation can be implied in scope
  (is (= (deps-satisfied? [] [0 :user/user-id]) true))
  (is (= (deps-satisfied? [0 :user/user-id] []) true))
  )

(deftest link-path-floor-1
  []
  ;(is (= (link-path-floor [0 :user/user-id]) [0]))
  ;(is (= (link-path-floor [0 :reg/gender]) [0]))
  ;(is (= (link-path-floor [0 :fiddle/links :link/fiddle]) [0 :fiddle/links]))
  )

;(deftest deps-over-satisfied-1
;  []
;  (is (= (deps-over-satisfied? [0 :fiddle/links] [0 :fiddle/links :link/fiddle]) true))
;  (is (= (deps-over-satisfied? [0 :gender] [0 :shirt-size]) true))
;  (is (= (deps-over-satisfied? [0] [0 :shirt-size]) true))
;  (is (= (deps-over-satisfied? [0] [0 :fiddle/links :link/fiddle]) true))
;  (is (= (deps-over-satisfied? [0 :fiddle/links] [0 :fiddle/links :link/fiddle]) true))
;  (is (= (deps-over-satisfied? [] [0]) true))
;  (is (= (deps-over-satisfied? [0] []) false))
;  (is (= (deps-over-satisfied? [] [0 :user/user-id]) true))
;  (is (= (deps-over-satisfied? [] [0 :user/user-id]) true))
;  (is (= (deps-over-satisfied? [0 :user/user-id] []) false))
;  )

(deftest newtest-1
  []
  ; !field(0) is oversatisfied for genders
  ;(is (= (deps-satisfied? [0] []) true))               ; body count is >=
  ;(is (= (deps-over-satisfied? [0] []) true))          ; left divergence has

  ; !field(0) is satisfied for shirt-sizes
  ;(is (= (deps-satisfied? [0] [0 :reg/gender]) true))  ; body count is =
  ;(is (= (deps-over-satisfied? [0] [0 :reg/gender]) false))
  ;(is (= (deps-over-satisfied? [0 :reg/gender] [0 :reg/gender]) true))
  ; It would be over-satisfied if there was another 
  )
