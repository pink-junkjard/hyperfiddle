(ns hypercrud.browser.context-test
  (:require
    [clojure.test :refer [deftest is testing]]
    [contrib.reactive :as r]
    [fixtures.tank]
    [hyperfiddle.data]
    [hyperfiddle.fiddle]
    [hypercrud.browser.context :as context]))


(defn mock-fiddle! [ident]
  (let [[fiddle result] (-> fixtures.tank/fiddles ident)]
    (context/fiddle
      {:hypercrud.browser/route nil}
      (r/pure fixtures.tank/schemas)
      (r/pure (hyperfiddle.fiddle/apply-defaults fiddle))
      (r/pure result))))

(deftest primitives
  []
  (def ctx (mock-fiddle! :dustingetz/gender-shirtsize))
  (def result (-> fixtures.tank/fiddles :hfnet.tank/index second))
  (testing "row keyfn on relation maps the smart identity"
    (is (= (context/row-keyfn ctx (first result))
           "17592186045419`13194139534712`true"))
    )
  )

(deftest basics
  (def ctx (mock-fiddle! :dustingetz/gender-shirtsize))

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
                            :dustingetz.reg/shirt-size #:db{:ident :dustingetz.shirt-size/mens-large}}})))

  (testing "fiddle level context is set"
    (is (= @(r/fmap :fiddle/ident (:hypercrud.browser/fiddle ctx))
           :dustingetz/gender-shirtsize))
    (is (= @(:hypercrud.browser/eav ctx)
           [nil :dustingetz/gender-shirtsize nil]))
    ; result-enclosure, schemas, qfind, link-index, validation

    (is (= @(:hypercrud.browser/result-enclosure ctx)
           [[:dustingetz.reg/email
             :dustingetz.reg/name                           ; name is absent from second row
             :db/id
             #:dustingetz.reg{:gender [:db/ident],
                              :shirt-size [:db/ident]}]])))
  )



(deftest context
  []

  (def ctx (mock-fiddle! :dustingetz/gender-shirtsize))

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
                            :dustingetz.reg/shirt-size #:db{:ident :dustingetz.shirt-size/mens-large}}})))

  (testing "refocus to self is noop and doesn't crash"
    (is (= (context/refocus ctx :dustingetz/gender-shirtsize)
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
           (let [[fiddle result] (-> fixtures.tank/fiddles :dustingetz/gender-shirtsize)]
             (* (count result)
                (count (keys (first result))))))))

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

  (testing "eav"
    (testing "fiddle level a is fiddle-ident"
      (is (= @(:hypercrud.browser/eav ctx)
             [nil :dustingetz/gender-shirtsize nil])))

    (testing "spread-result does not touch eav"
      (is (= [@(:hypercrud.browser/eav ctx)]
             (for [[_ ctx] (context/spread-result ctx)]
               @(:hypercrud.browser/eav ctx))
             '([nil :dustingetz/gender-shirtsize nil]))))

    (testing "fiddle-level v is the element"
      (is (= (for [[_ ctx] (context/spread-rows ctx)
                   [_ ctx] (context/spread-elements ctx)]
               @(:hypercrud.browser/eav ctx))
             (for [[_ ctx] (context/spread-result ctx)
                   [_ ctx] (context/spread-rows ctx)
                   [_ ctx] (context/spread-elements ctx)]
               @(:hypercrud.browser/eav ctx))
             (for [[_ ctx] (context/spread-result ctx)
                   [_ ctx] (context/spread-rows ctx)
                   [_ ctx] (context/spread-elements ctx)]
               (context/eav ctx))
             '([nil :dustingetz/gender-shirtsize 17592186046196]
                [nil :dustingetz/gender-shirtsize 17592186046763]))))

    (testing "spread-row doesnt prefill v, but it can be inferred"
      (is (= (for [[_ ctx] (context/spread-rows ctx)]
               @(:hypercrud.browser/eav ctx))
             '([nil :dustingetz/gender-shirtsize nil]
                [nil :dustingetz/gender-shirtsize nil])))

      (is (= (for [[_ ctx] (context/spread-rows ctx)]
               (context/eav ctx))
             '([nil :dustingetz/gender-shirtsize 17592186046196]
                [nil :dustingetz/gender-shirtsize 17592186046763]))))

    (is (= (for [[_ ctx] (context/spread-result ctx)
                 [_ ctx] (context/spread-rows ctx)
                 [_ ctx] (context/spread-elements ctx)]
             @(:hypercrud.browser/eav ctx))
           '([nil :dustingetz/gender-shirtsize 17592186046196]
              [nil :dustingetz/gender-shirtsize 17592186046763])))

    )

  (testing "fiddle/type :pull"
    (testing "row and element are inferred"
      (is (= (for [ctx [(mock-fiddle! :hyperfiddle/ide)]
                   [_ ctx] (context/spread-rows ctx)
                   [_ ctx] (context/spread-elements ctx)]
               (context/eav ctx))
             (for [ctx [(mock-fiddle! :hyperfiddle/ide)]]
               (context/eav ctx))
             [[nil :hyperfiddle/ide 17592186061847]]
             ))

      (is (= (let [ctx (mock-fiddle! :hyperfiddle/ide)
                   ctx (context/attribute ctx :fiddle/renderer)]
               (context/eav ctx))
             [17592186061847 :fiddle/renderer "hyperfiddle.ide.fiddles.fiddle-src/fiddle-src-renderer"])))
    )

  (testing "nested pulls"
    (let [ctx (mock-fiddle! :hyperfiddle/ide)]
      (is (= (context/eav ctx) [nil :hyperfiddle/ide 17592186061847]))
      (let [ctx (context/attribute ctx :fiddle/links)]
        (is (= (context/eav ctx) [17592186061847 :fiddle/links nil]))
        (is (= (count @(context/data ctx)) 5))
        (is (= (first @(context/data ctx)) {:db/id 17592186061848, :link/class [:hf/remove], :link/rel :hf/remove}))
        (let [ctx (context/row ctx 17592186061848)]
          (is (= (context/eav ctx) [17592186061847 :fiddle/links 17592186061848]))
          (is (= @(context/data ctx) {:db/id 17592186061848, :link/class [:hf/remove], :link/rel :hf/remove})))))

    )





  (testing "a is fiddle-ident if no element set"
    (is (= (for [[_ ctx] (context/spread-result ctx)
                 [_ ctx] (context/spread-rows ctx)]
             ; Element got inferred here
             @(context/data ctx))
           [{:db/id 17592186046196,
             :dustingetz.reg/email "dustin@example.com",
             :dustingetz.reg/name "Dustin Getz",
             :dustingetz.reg/gender #:db{:ident :dustingetz.gender/male},
             :dustingetz.reg/shirt-size #:db{:ident :dustingetz.shirt-size/mens-large}}
            {:db/id 17592186046763,
             :dustingetz.reg/email "bob@example.com",
             :dustingetz.reg/gender #:db{:ident :dustingetz.gender/male},
             :dustingetz.reg/shirt-size #:db{:ident :dustingetz.shirt-size/mens-large}}])))

  (testing "refocus"
    (is (= (for [[_ ctx] (context/spread-result ctx)
                 [_ ctx] (context/spread-rows ctx)]
             @(:hypercrud.browser/eav (context/refocus ctx :dustingetz/gender-shirtsize)))
           '([nil :dustingetz/gender-shirtsize nil]
              [nil :dustingetz/gender-shirtsize nil])))

    (is (= (for [[_ ctx] (context/spread-result ctx)
                 [_ ctx] (context/spread-rows ctx)
                 [_ ctx] (context/spread-elements ctx)]
             ; Focusing the element
             @(:hypercrud.browser/eav ctx #_(context/refocus ctx :dustingetz/gender-shirtsize)))
           '([nil :dustingetz/gender-shirtsize 17592186046196]
              [nil :dustingetz/gender-shirtsize 17592186046763])))

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
             [nil :dustingetz/gender-shirtsize nil])))

    (testing "spread rows"
      (testing "target isolated row"
        (is (= (for [ctx [(context/row ctx 17592186046196)]]
                 ; infers element
                 @(context/data ctx))
               (for [ctx [(context/row ctx 17592186046196)]
                     [_ ctx] (context/spread-elements ctx)]
                 @(context/data ctx))
               [{:db/id 17592186046196,
                 :dustingetz.reg/email "dustin@example.com",
                 :dustingetz.reg/name "Dustin Getz",
                 :dustingetz.reg/gender #:db{:ident :dustingetz.gender/male},
                 :dustingetz.reg/shirt-size #:db{:ident :dustingetz.shirt-size/mens-large}}])))

      (testing "target all rows"
        (is (= (for [[_ ctx] (context/spread-rows ctx)]
                 @(context/data ctx))
               (for [[_ ctx] (context/spread-rows ctx)
                     [_ ctx] (context/spread-elements ctx)]
                 @(context/data ctx))
               [{:db/id 17592186046196,
                 :dustingetz.reg/email "dustin@example.com",
                 :dustingetz.reg/name "Dustin Getz",
                 :dustingetz.reg/gender #:db{:ident :dustingetz.gender/male},
                 :dustingetz.reg/shirt-size #:db{:ident :dustingetz.shirt-size/mens-large}}
                {:db/id 17592186046763,
                 :dustingetz.reg/email "bob@example.com",
                 :dustingetz.reg/gender #:db{:ident :dustingetz.gender/male},
                 :dustingetz.reg/shirt-size #:db{:ident :dustingetz.shirt-size/mens-large}}])))

      (testing "focus attribute isolated"
        (is (= (let [ctx (context/row ctx 17592186046196)
                     ctx (context/element ctx 0)
                     ctx (context/attribute ctx :dustingetz.reg/gender)]
                 @(context/data ctx))
               #:db{:ident :dustingetz.gender/male})))

      (testing "FindColl Pull nested"
        (testing "eav"
          (is (= (let [ctx (mock-fiddle! :tutorial.race/shirt-sizes)
                       ctx (context/row ctx :dustingetz.shirt-size/womens-medium)]
                   (context/eav ctx))
                 [nil :tutorial.race/shirt-sizes :dustingetz.shirt-size/womens-medium]))

          (is (= (let [ctx (mock-fiddle! :tutorial.race/shirt-sizes)
                       ctx (context/row ctx :dustingetz.shirt-size/womens-medium)
                       ctx (context/attribute ctx :dustingetz.reg/gender)]
                   (context/eav ctx))
                 [:dustingetz.shirt-size/womens-medium :dustingetz.reg/gender :dustingetz.gender/female]))

          (is (= (let [ctx (mock-fiddle! :tutorial.race/shirt-sizes)
                       ctx (context/row ctx :dustingetz.shirt-size/womens-medium)
                       ctx (context/attribute ctx :db/ident)]
                   (context/eav ctx))
                 [:dustingetz.shirt-size/womens-medium :db/ident :dustingetz.shirt-size/womens-medium])))

        (testing "data"
          (is (= (let [ctx (mock-fiddle! :tutorial.race/shirt-sizes)
                       ctx (context/row ctx :dustingetz.shirt-size/womens-medium)
                       ctx (context/attribute ctx :dustingetz.reg/gender)]
                   @(context/data ctx))
                 #:db{:ident :dustingetz.gender/female}))

          (is (= (let [ctx (mock-fiddle! :tutorial.race/shirt-sizes)
                       ctx (context/row ctx :dustingetz.shirt-size/womens-medium)
                       ctx (context/attribute ctx :db/ident)]
                   @(context/data ctx))
                 :dustingetz.shirt-size/womens-medium))

          (is (= (let [ctx (mock-fiddle! :tutorial.race/shirt-sizes)
                       ctx (context/row ctx :dustingetz.shirt-size/womens-medium)]
                   @(context/data ctx))
                 {:db/ident :dustingetz.shirt-size/womens-medium,
                  :dustingetz.reg/gender #:db{:ident :dustingetz.gender/female}}))

          (is (= (let [ctx (mock-fiddle! :tutorial.race/shirt-sizes)]
                   @(context/data ctx))
                 [{:db/ident :dustingetz.shirt-size/womens-medium, :dustingetz.reg/gender #:db{:ident :dustingetz.gender/female}}
                  {:db/ident :dustingetz.shirt-size/womens-small, :dustingetz.reg/gender #:db{:ident :dustingetz.gender/female}}
                  {:db/ident :dustingetz.shirt-size/womens-large, :dustingetz.reg/gender #:db{:ident :dustingetz.gender/female}}]))
          )
        (testing "element inference"
          (is (= (let [ctx (mock-fiddle! :tutorial.race/shirt-sizes)
                       ctx (context/row ctx :dustingetz.shirt-size/womens-medium)
                       ctx (context/attribute ctx :db/ident)]
                   @(context/data ctx))
                 :dustingetz.shirt-size/womens-medium))

          (is (= (let [ctx (mock-fiddle! :tutorial.race/shirt-sizes)
                       ctx (context/row ctx :dustingetz.shirt-size/womens-medium)]
                   ; infer element
                   @(context/data ctx))
                 {:db/ident :dustingetz.shirt-size/womens-medium, :dustingetz.reg/gender #:db{:ident :dustingetz.gender/female}})))
        )

      (testing "head-sentinel sanity checks, head-sentinel needs to go"
        (let [ctx (mock-fiddle! :seattle/neighborhoods)]
          (is (= (context/eav ctx) [nil :seattle/neighborhoods nil]))
          (is (= (count @(context/data ctx)) 6))
          (let [ctx (context/attribute ctx :neighborhood/district)]
            (is (= (context/eav ctx) [nil :neighborhood/district nil])))
          (let [ctx (context/row ctx 17592186045522)
                ctx (context/attribute ctx :neighborhood/district)]
            (is (= (context/eav ctx) [17592186045522 :neighborhood/district 17592186045521])))
          (testing "if head-sentinel, no v"
            (let [ctx (assoc ctx :hypercrud.browser/head-sentinel true)
                  ctx (context/row ctx 17592186045522)
                  ctx (context/attribute ctx :neighborhood/district)]
              (is (= (context/eav ctx) [nil :neighborhood/district nil]))))))

      (testing "refocus"

        (testing "to :ref :one from row"
          (is (= (let [ctx (context/row ctx 17592186046196)
                       ctx (context/element ctx 0)
                       ctx (context/refocus ctx :dustingetz.reg/gender)]
                   @(context/data ctx))
                 #:db{:ident :dustingetz.gender/male}))

          (is (= (for [[_ ctx] (context/spread-rows ctx)
                       [_ ctx] (context/spread-elements ctx)]
                   (let [ctx (context/refocus ctx :dustingetz.reg/gender)]
                     @(context/data ctx)))
                 (for [[_ ctx] (context/spread-rows ctx)]
                   ; infer element
                   (let [ctx (context/refocus ctx :dustingetz.reg/gender)]
                     @(context/data ctx)))
                 [#:db{:ident :dustingetz.gender/male}
                  #:db{:ident :dustingetz.gender/male}]))

          (is (= (let [ctx (context/row ctx 17592186046196)
                       ctx (context/element ctx 0)
                       ctx (context/refocus ctx :dustingetz.reg/gender)]
                   @(:hypercrud.browser/eav ctx))
                 (let [ctx (context/row ctx 17592186046196)
                       ; infer element
                       ctx (context/refocus ctx :dustingetz.reg/gender)]
                   @(:hypercrud.browser/eav ctx))
                 [17592186046196 :dustingetz.reg/gender :dustingetz.gender/male]))

          (is (= (for [[_ ctx] (context/spread-rows ctx)
                       [_ ctx] (context/spread-elements ctx)]
                   (let [ctx (context/refocus ctx :dustingetz.reg/gender)]
                     @(:hypercrud.browser/eav ctx)))
                 (for [[_ ctx] (context/spread-rows ctx)]
                   ; infer elements
                   (let [ctx (context/refocus ctx :dustingetz.reg/gender)]
                     @(:hypercrud.browser/eav ctx)))
                 '([17592186046196 :dustingetz.reg/gender :dustingetz.gender/male]
                    [17592186046763 :dustingetz.reg/gender :dustingetz.gender/male]))))

        (testing "to :ref :many from row")

        (testing "to :ref :one from top"
          (is (= (let [ctx (context/refocus ctx :dustingetz.reg/gender)]
                   ; Can't do it. Should this throw?
                   @(context/data ctx))
                 nil))

          (is (= (for [[_ ctx] (context/spread-rows ctx)
                       [_ ctx] (context/spread-elements ctx)]
                   (let [ctx (context/refocus ctx :dustingetz.reg/gender)]
                     @(context/data ctx)))
                 [#:db{:ident :dustingetz.gender/male}
                  #:db{:ident :dustingetz.gender/male}]))

          (is (= (let [ctx (context/row ctx 17592186046196)
                       ctx (context/element ctx 0)
                       ctx (context/refocus ctx :dustingetz.reg/gender)]
                   @(:hypercrud.browser/eav ctx))
                 [17592186046196 :dustingetz.reg/gender :dustingetz.gender/male]))

          (is (= (for [[_ ctx] (context/spread-rows ctx)
                       [_ ctx] (context/spread-elements ctx)]
                   (let [ctx (context/refocus ctx :dustingetz.reg/gender)]
                     @(:hypercrud.browser/eav ctx)))
                 '([17592186046196 :dustingetz.reg/gender :dustingetz.gender/male]
                    [17592186046763 :dustingetz.reg/gender :dustingetz.gender/male]))))
        )

      #_(testing "refocus from root to gender"
        (is (= (for [[_ ctx] (context/spread-rows ctx)
                     #_#_[_ ctx] (context/spread-elements ctx)]
                 #_@(:hypercrud.browser/result-index ctx)
                 @(:hypercrud.browser/result ctx)
                 #_@(:hypercrud.browser/result-index (context/refocus ctx :dustingetz.reg/gender))
                 )
               '({17592186046196 {:db/id 17592186046196,
                                  :dustingetz.reg/email "dustin@example.com",
                                  :dustingetz.reg/name "Dustin Getz",
                                  :dustingetz.reg/gender #:db{:ident :dustingetz.gender/male},
                                  :dustingetz.reg/shirt-size #:db{:ident :dustingetz.shirt-size/mens-large}},
                  17592186046763 {:db/id 17592186046763,
                                  :dustingetz.reg/email "bob@example.com",
                                  :dustingetz.reg/gender #:db{:ident :dustingetz.gender/male},
                                  :dustingetz.reg/shirt-size #:db{:ident :dustingetz.shirt-size/mens-large}}}
                  {17592186046196 {:db/id 17592186046196,
                                   :dustingetz.reg/email "dustin@example.com",
                                   :dustingetz.reg/name "Dustin Getz",
                                   :dustingetz.reg/gender #:db{:ident :dustingetz.gender/male},
                                   :dustingetz.reg/shirt-size #:db{:ident :dustingetz.shirt-size/mens-large}},
                   17592186046763 {:db/id 17592186046763,
                                   :dustingetz.reg/email "bob@example.com",
                                   :dustingetz.reg/gender #:db{:ident :dustingetz.gender/male},
                                   :dustingetz.reg/shirt-size #:db{:ident :dustingetz.shirt-size/mens-large}}})))

        (is (= (for [[_ ctx] (context/spread-rows ctx)
                     [_ ctx] (context/spread-elements ctx)]
                 @(:hypercrud.browser/eav (context/refocus ctx :dustingetz.reg/gender)))
               '([17592186046196 :dustingetz.reg/gender :dustingetz.gender/male]
                  [17592186046763 :dustingetz.reg/gender :dustingetz.gender/male])))
        )

      (testing "refocus from myself when already here but not at root")
      )
    )
  )

(deftest links
  []


  (testing "race"
    (def ctx (mock-fiddle! :tutorial.race/submission))
    (is (-> @(hyperfiddle.data/select-many ctx :dustingetz.reg/gender)
            first :link/fiddle :fiddle/ident (= :tutorial.race/shirt-sizes)))
    ;(println @(hyperfiddle.data/select-many ctx :tutorial.race/submission))
    @(hyperfiddle.data/select-many ctx :hf/iframe)


    @(:hypercrud.browser/link-index ctx)

    )

  (testing "seattle iframes"
    (def ctx (mock-fiddle! :seattle/neighborhoods))
    (-> @(hyperfiddle.data/select-many-here ctx :seattle/districts)
        first :link/fiddle :fiddle/ident (= :seattle/districts))




    #_(testing "if head-sentinel, no v"
        (let [ctx (assoc ctx :hypercrud.browser/head-sentinel true)
              ctx (context/row ctx 17592186045522)
              ctx (context/attribute ctx :neighborhood/district)]
          (is (= (context/eav ctx) [nil :neighborhood/district nil])))))

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
