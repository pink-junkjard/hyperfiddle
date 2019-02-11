(ns hypercrud.browser.context-test
  (:require
    [cats.core :refer [mlet return fmap extract]]
    [cats.monad.either :refer [left right left? right?]]
    [clojure.test :refer [deftest is testing]]
    [contrib.reactive :as r]
    [contrib.ct :refer [unwrap]]
    [fixtures.tank]
    [fixtures.domains]
    [hyperfiddle.core]                                      ; avoid cycle hyperfiddle.api
    [hyperfiddle.data]
    [hyperfiddle.fiddle]
    [hyperfiddle.reducers]
    [hyperfiddle.runtime]
    [hypercrud.browser.context :as context]))


(defn mock-peer []
  ; Stop NPEs in tempid that inspect the stage.
  (let [state-atom (-> (hyperfiddle.reducers/root-reducer nil nil)
                       (r/atom))]
    (reify
      hyperfiddle.runtime/State
      (state [rt] state-atom)
      (state [rt path] (r/cursor state-atom path)))))

(defn mock-fiddle! "This has some subtle differences around tempid handling - this mock
  has no information about tempids at all, so these tests do not exercise tempid paths which
  can subtly influence keyfns possibly other things."
  ([ident] (mock-fiddle! fixtures.tank/schemas fixtures.tank/fiddles ident))
  ([schemas fiddles ident]
   (let [[fiddle result] (-> fiddles ident)]
     (-> {:hypercrud.browser/route nil
          :peer (mock-peer)}
         (context/schemas (r/pure schemas))
         (context/fiddle (r/pure fiddle))
         (context/result (r/pure result))))))

(deftest primitives
  []
  (testing "row keyfn on relation maps the smart identity"
    (def ctx (mock-fiddle! :hfnet.tank/index))
    (def result (context/data ctx))
    (is (= (context/row-key ctx (first result))
           [[:fiddle/ident :karl.hardenstine/fiddles] 13194139534712 true])))

  (testing "key-row"
    (let [ctx (mock-fiddle! :dustingetz/gender-shirtsize)
          [row] (context/data ctx)]
      (is (= (context/row-key ctx row)
             [:dustingetz.reg/email "dustin@example.com"]))))

  (testing "from :identity refine to :db/id"
    (is (= (let [ctx (mock-fiddle! :dustingetz/gender-shirtsize)
                 ctx (context/row ctx [:dustingetz.reg/email "dustin@example.com"])
                 ctx (context/attribute ctx :db/id)]
             (context/eav ctx))
           [[:dustingetz.reg/email "dustin@example.com"] :db/id 17592186046196])))
  )

(deftest basics
  (def ctx (mock-fiddle! :dustingetz/gender-shirtsize))

  (testing "row indexing"
    (testing "data sidesteps indexing, userland never sees indexing."
      (is (= (let [ctx (mock-fiddle! :dustingetz/gender-shirtsize)]
               (context/data ctx))
             [{:db/id 17592186046196,
               :dustingetz.reg/email "dustin@example.com",
               :dustingetz.reg/name "Dustin Getz",
               :dustingetz.reg/gender #:db{:ident :dustingetz.gender/male},
               :dustingetz.reg/shirt-size #:db{:ident :dustingetz.shirt-size/mens-large}}
              {:db/id 17592186046763,
               :dustingetz.reg/email "bob@example.com",
               :dustingetz.reg/gender #:db{:ident :dustingetz.gender/male},
               :dustingetz.reg/shirt-size #:db{:ident :dustingetz.shirt-size/mens-large}}])))

    (testing "FindColl rows are indexed by smart identity (only visible internally)"
      (is (= (let [ctx (mock-fiddle! :dustingetz/gender-shirtsize)]
               @(:hypercrud.browser/result-index ctx))
             {[:dustingetz.reg/email "dustin@example.com"]
              {:db/id 17592186046196,
               :dustingetz.reg/email "dustin@example.com",
               :dustingetz.reg/name "Dustin Getz",
               :dustingetz.reg/gender #:db{:ident :dustingetz.gender/male},
               :dustingetz.reg/shirt-size #:db{:ident :dustingetz.shirt-size/mens-large}},
              [:dustingetz.reg/email "bob@example.com"]
              {:db/id 17592186046763,
               :dustingetz.reg/email "bob@example.com",
               :dustingetz.reg/gender #:db{:ident :dustingetz.gender/male},
               :dustingetz.reg/shirt-size #:db{:ident :dustingetz.shirt-size/mens-large}}}))))

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

(def ctx (mock-fiddle! :dustingetz/gender-shirtsize))

(deftest simple

  (testing "refocus to self is noop and doesn't crash"
    (def ctx (mock-fiddle! :dustingetz/gender-shirtsize))
    (is (= (extract (context/refocus+ ctx :dustingetz/gender-shirtsize))
           ctx)))

  (testing "refocus to dependent didn't crash"
    (def ctx (mock-fiddle! :dustingetz/gender-shirtsize))
    (is (= @(:hypercrud.browser/eav (extract (context/refocus+ ctx :dustingetz.reg/gender)))
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

  (testing "fiddle level a is fiddle-ident"
    (is (= @(:hypercrud.browser/eav ctx)
           [nil :dustingetz/gender-shirtsize nil])))

  (testing "spread-result does not touch eav"
    (is (= [@(:hypercrud.browser/eav ctx)]
           (for [[_ ctx] (context/spread-result ctx)]
             @(:hypercrud.browser/eav ctx))
           '([nil :dustingetz/gender-shirtsize nil]))))
  )

(deftest context-a
  (def ctx (mock-fiddle! :dustingetz/gender-shirtsize))

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
           '([nil :dustingetz/gender-shirtsize [:dustingetz.reg/email "dustin@example.com"]]
              [nil :dustingetz/gender-shirtsize [:dustingetz.reg/email "bob@example.com"]]))))

  (testing "spread-row doesnt prefill v, but it can be inferred"
    (is (= #_(for [[_ ctx] (context/spread-rows ctx)]
               @(:hypercrud.browser/eav ctx))               ; element not inferred, why no lookup ref? Because no schema.
          (for [[_ ctx] (context/spread-rows ctx)]
            (context/eav ctx))
          [[nil :dustingetz/gender-shirtsize [:dustingetz.reg/email "dustin@example.com"]]
           [nil :dustingetz/gender-shirtsize [:dustingetz.reg/email "bob@example.com"]]]))

    (is (= (for [[_ ctx] (context/spread-rows ctx)]
             (context/eav ctx))
           '([nil :dustingetz/gender-shirtsize [:dustingetz.reg/email "dustin@example.com"]]
              [nil :dustingetz/gender-shirtsize [:dustingetz.reg/email "bob@example.com"]]))))

  (is (= (for [[_ ctx] (context/spread-result ctx)
               [_ ctx] (context/spread-rows ctx)
               [_ ctx] (context/spread-elements ctx)]
           @(:hypercrud.browser/eav ctx))
         '([nil :dustingetz/gender-shirtsize [:dustingetz.reg/email "dustin@example.com"]]
            [nil :dustingetz/gender-shirtsize [:dustingetz.reg/email "bob@example.com"]]))))

(deftest context-c
  (testing "fiddle/type :pull"
    (is (= (for [ctx [(mock-fiddle! :hyperfiddle/ide)]
                 [_ ctx] (context/spread-rows ctx)
                 [_ ctx] (context/spread-elements ctx)]
             (context/eav ctx))
           (for [ctx [(mock-fiddle! :hyperfiddle/ide)]]
             (context/eav ctx))
           [[nil :hyperfiddle/ide [:fiddle/ident :hyperfiddle/ide]]]))

    (is (= (let [ctx (mock-fiddle! :hyperfiddle/ide)
                 ctx (context/attribute ctx :fiddle/renderer)]
             (context/eav ctx))
           [[:fiddle/ident :hyperfiddle/ide] :fiddle/renderer "hyperfiddle.ide.fiddles.fiddle-src/fiddle-src-renderer"]))))

(deftest context-b
  (def ctx (mock-fiddle! :dustingetz/gender-shirtsize))
  (testing "nested pulls"
    (let [ctx (mock-fiddle! :hyperfiddle/ide)]
      (is (= (context/eav ctx) [nil :hyperfiddle/ide [:fiddle/ident :hyperfiddle/ide]])) ; infer scalar
      ;(is (= (context/data ctx) 42))
      (testing "nested ref many"
        (let [ctx (context/attribute ctx :fiddle/links)]
          (is (= (context/eav ctx) [[:fiddle/ident :hyperfiddle/ide] :fiddle/links nil]))
          (is (= (count (context/data ctx)) 5))
          (is (= (first (context/data ctx)) {:db/id 17592186061848, :link/class [:hf/remove], :link/rel :hf/remove}))
          (let [ctx (context/row ctx 17592186061848)]
            (is (= (context/eav ctx) [[:fiddle/ident :hyperfiddle/ide] :fiddle/links 17592186061848]))
            (is (= (context/data ctx) {:db/id 17592186061848, :link/class [:hf/remove], :link/rel :hf/remove})))))

      (testing "nested scalar many"
        (let [ctx (context/attribute ctx :hyperfiddle/owners)]
          (is (= (context/eav ctx) [[:fiddle/ident :hyperfiddle/ide] :hyperfiddle/owners nil]))
          (is (= (context/data ctx) [#uuid "acd054a8-4e36-4d6c-a9ec-95bdc47f0d39"]))
          (let [ctx (context/row ctx #uuid "acd054a8-4e36-4d6c-a9ec-95bdc47f0d39")]
            (is (= (context/eav ctx)
                   [[:fiddle/ident :hyperfiddle/ide] :hyperfiddle/owners #uuid "acd054a8-4e36-4d6c-a9ec-95bdc47f0d39"])))
          ))))

  (testing "nested pull on :hyperfiddle/ide :fiddle/links"
    (let [ctx (mock-fiddle! :hyperfiddle/ide)]
      (is (= (context/eav ctx) [nil :hyperfiddle/ide [:fiddle/ident :hyperfiddle/ide]]))
      (let [ctx (context/attribute ctx :fiddle/links)]
        (is (= (context/eav ctx) [[:fiddle/ident :hyperfiddle/ide] :fiddle/links nil]))
        (is (= (count (context/data ctx)) 5))
        (is (= (first (context/data ctx)) {:db/id 17592186061848, :link/class [:hf/remove], :link/rel :hf/remove}))
        (let [ctx (context/row ctx 17592186061848)]
          (is (= (context/eav ctx) [[:fiddle/ident :hyperfiddle/ide] :fiddle/links 17592186061848]))
          (is (= (context/data ctx) {:db/id 17592186061848, :link/class [:hf/remove], :link/rel :hf/remove}))))))

  (testing "a is fiddle-ident if no element set"
    (is (= (for [[_ ctx] (context/spread-result ctx)
                 [_ ctx] (context/spread-rows ctx)]
             ; Element got inferred here
             (context/data ctx))
           [{:db/id 17592186046196,
             :dustingetz.reg/email "dustin@example.com",
             :dustingetz.reg/name "Dustin Getz",
             :dustingetz.reg/gender #:db{:ident :dustingetz.gender/male},
             :dustingetz.reg/shirt-size #:db{:ident :dustingetz.shirt-size/mens-large}}
            {:db/id 17592186046763,
             :dustingetz.reg/email "bob@example.com",
             :dustingetz.reg/gender #:db{:ident :dustingetz.gender/male},
             :dustingetz.reg/shirt-size #:db{:ident :dustingetz.shirt-size/mens-large}}])))

  (testing "refocusing from a row doesn't lose the row"
    (is (= (for [[_ ctx] (-> (mock-fiddle! :dustingetz/gender-shirtsize)
                             (context/spread-result))
                 [_ ctx] (context/spread-rows ctx)]
             (context/eav (extract (context/refocus+ ctx :dustingetz/gender-shirtsize))))
           [[nil :dustingetz/gender-shirtsize [:dustingetz.reg/email "dustin@example.com"]]
            [nil :dustingetz/gender-shirtsize [:dustingetz.reg/email "bob@example.com"]]])))

  (testing "refocus"
    (is (= (for [[_ ctx] (context/spread-result ctx)
                 [_ ctx] (context/spread-rows ctx)
                 [_ ctx] (context/spread-elements ctx)]
             ; Focusing the element
             @(:hypercrud.browser/eav ctx #_(extract (context/refocus+ ctx :dustingetz/gender-shirtsize))))
           '([nil :dustingetz/gender-shirtsize [:dustingetz.reg/email "dustin@example.com"]]
              [nil :dustingetz/gender-shirtsize [:dustingetz.reg/email "bob@example.com"]])))

    (is (= (for [[_ ctx] (context/spread-result ctx)
                 [_ ctx] (context/spread-rows ctx)]
             @(:hypercrud.browser/eav
                (extract (context/refocus+ ctx :dustingetz.reg/gender))))
           '([[:dustingetz.reg/email "dustin@example.com"] :dustingetz.reg/gender :dustingetz.gender/male]
              [[:dustingetz.reg/email "bob@example.com"] :dustingetz.reg/gender :dustingetz.gender/male])))
    )

  )

(deftest context-d
  (testing "focus attribute isolated"
    (def ctx (mock-fiddle! :dustingetz/gender-shirtsize))
    (is (= (let [ctx (context/row ctx [:dustingetz.reg/email "dustin@example.com"] #_17592186046196)
                 ctx (context/element ctx 0)
                 ctx (context/attribute ctx :dustingetz.reg/gender)]
             (context/data ctx))
           #:db{:ident :dustingetz.gender/male})))

  (testing "FindColl Pull nested"
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
                 ;ctx (context/element ctx 0)
                 ctx (context/attribute ctx :db/ident)]
             (context/eav ctx))
           [:dustingetz.shirt-size/womens-medium :db/ident :dustingetz.shirt-size/womens-medium])))

  #_(testing "refocus from root to gender"
      (is (= (for [[_ ctx] (context/spread-rows ctx)
                   #_#_[_ ctx] (context/spread-elements ctx)]
               #_@(:hypercrud.browser/result-index ctx)
               @(:hypercrud.browser/result ctx)
               #_@(:hypercrud.browser/result-index (extract (context/refocus+ ctx :dustingetz.reg/gender)))
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
               @(:hypercrud.browser/eav (extract (context/refocus+ ctx :dustingetz.reg/gender))))
             '([17592186046196 :dustingetz.reg/gender :dustingetz.gender/male]
                [17592186046763 :dustingetz.reg/gender :dustingetz.gender/male])))
      ))

(deftest element-inference
  (def ctx (mock-fiddle! :dustingetz/gender-shirtsize))
  (testing "element inference"
    (is (= (let [ctx (mock-fiddle! :tutorial.race/shirt-sizes)
                 ctx (context/row ctx :dustingetz.shirt-size/womens-medium)
                 ctx (context/attribute ctx :db/ident)]
             (context/data ctx))
           :dustingetz.shirt-size/womens-medium))

    (is (= (let [ctx (mock-fiddle! :tutorial.race/shirt-sizes)
                 ctx (context/row ctx :dustingetz.shirt-size/womens-medium)]
             ; infer element
             (context/data ctx))
           {:db/ident :dustingetz.shirt-size/womens-medium, :dustingetz.reg/gender #:db{:ident :dustingetz.gender/female}}))))

(deftest data
  (def ctx (mock-fiddle! :dustingetz/gender-shirtsize))
  (testing "data"
    (is (= (let [ctx (mock-fiddle! :tutorial.race/shirt-sizes)
                 ctx (context/row ctx :dustingetz.shirt-size/womens-medium)
                 ctx (context/attribute ctx :dustingetz.reg/gender)]
             (context/data ctx))
           #:db{:ident :dustingetz.gender/female}))

    (is (= (let [ctx (mock-fiddle! :tutorial.race/shirt-sizes)
                 ctx (context/row ctx :dustingetz.shirt-size/womens-medium)
                 ctx (context/attribute ctx :db/ident)]
             (context/data ctx))
           :dustingetz.shirt-size/womens-medium))

    (is (= (let [ctx (mock-fiddle! :tutorial.race/shirt-sizes)
                 ctx (context/row ctx :dustingetz.shirt-size/womens-medium)]
             (context/data ctx))
           {:db/ident :dustingetz.shirt-size/womens-medium,
            :dustingetz.reg/gender #:db{:ident :dustingetz.gender/female}}))

    (is (= (let [ctx (mock-fiddle! :tutorial.race/shirt-sizes)]
             (context/data ctx))
           [{:db/ident :dustingetz.shirt-size/womens-medium, :dustingetz.reg/gender #:db{:ident :dustingetz.gender/female}}
            {:db/ident :dustingetz.shirt-size/womens-small, :dustingetz.reg/gender #:db{:ident :dustingetz.gender/female}}
            {:db/ident :dustingetz.shirt-size/womens-large, :dustingetz.reg/gender #:db{:ident :dustingetz.gender/female}}]))
    ))

(deftest rows
  (def ctx (mock-fiddle! :dustingetz/gender-shirtsize))
  (testing "row data by dbid"
    #_(:hypercrud.browser/result-path (context/row ctx 17592186046196))
    #_(:hypercrud.browser/result (context/row ctx 17592186046196))
    ; not sure if element can be inferred in data. Row does not infer.
    (is (= (context/data (-> ctx (context/element 0) (context/row [:dustingetz.reg/email "dustin@example.com"] #_17592186046196)))
           {:db/id 17592186046196,
            :dustingetz.reg/email "dustin@example.com",
            :dustingetz.reg/name "Dustin Getz",
            :dustingetz.reg/gender #:db{:ident :dustingetz.gender/male},
            :dustingetz.reg/shirt-size #:db{:ident :dustingetz.shirt-size/mens-large}}))
    )

  (testing "row does not set e"
    (is (= @(:hypercrud.browser/eav (context/row ctx 17592186046196))
           [nil :dustingetz/gender-shirtsize nil])))
  (testing "target isolated row"
    (is (= (for [ctx [(context/row ctx [:dustingetz.reg/email "dustin@example.com"] #_17592186046196)]]
             ; infers element
             (context/data ctx))
           (for [ctx [(context/row ctx [:dustingetz.reg/email "dustin@example.com"] #_17592186046196)]
                 [_ ctx] (context/spread-elements ctx)]
             (context/data ctx))
           [{:db/id 17592186046196,
             :dustingetz.reg/email "dustin@example.com",
             :dustingetz.reg/name "Dustin Getz",
             :dustingetz.reg/gender #:db{:ident :dustingetz.gender/male},
             :dustingetz.reg/shirt-size #:db{:ident :dustingetz.shirt-size/mens-large}}])))

  (testing "target all rows"
    (is (= (for [[_ ctx] (context/spread-rows ctx)]
             (context/data ctx))
           (for [[_ ctx] (context/spread-rows ctx)
                 [_ ctx] (context/spread-elements ctx)]
             (context/data ctx))
           [{:db/id 17592186046196,
             :dustingetz.reg/email "dustin@example.com",
             :dustingetz.reg/name "Dustin Getz",
             :dustingetz.reg/gender #:db{:ident :dustingetz.gender/male},
             :dustingetz.reg/shirt-size #:db{:ident :dustingetz.shirt-size/mens-large}}
            {:db/id 17592186046763,
             :dustingetz.reg/email "bob@example.com",
             :dustingetz.reg/gender #:db{:ident :dustingetz.gender/male},
             :dustingetz.reg/shirt-size #:db{:ident :dustingetz.shirt-size/mens-large}}]))))

(deftest head-sentinel
  (def ctx (mock-fiddle! :dustingetz/gender-shirtsize))
  (testing "head-sentinel sanity checks, head-sentinel needs to go"
    (let [ctx (mock-fiddle! :seattle/neighborhoods)]
      (is (= (context/eav ctx) [nil :seattle/neighborhoods nil]))
      (is (= (count (context/data ctx)) 7))
      (let [ctx (context/attribute ctx :neighborhood/district)]
        (is (= (context/eav ctx) [nil :neighborhood/district nil])))
      (let [ctx (context/row ctx [:neighborhood/name "Admiral (West Seattle)"] #_17592186045522)
            ctx (context/attribute ctx :neighborhood/district)]
        (is (= (context/eav ctx)
               [[:neighborhood/name "Admiral (West Seattle)"] :neighborhood/district [:district/name "Southwest"]])))
      (testing "if head-sentinel, no v"
        (let [ctx (assoc ctx :hypercrud.browser/head-sentinel true)
              ctx (context/row ctx 17592186045522)
              ctx (context/attribute ctx :neighborhood/district)]
          (is (= (context/eav ctx) [nil :neighborhood/district nil]))))))
  )

(deftest refocus-a
  (def ctx (mock-fiddle! :dustingetz/gender-shirtsize))
  (testing "refocus to :ref :one from row"
    (is (= (let [ctx (context/row ctx [:dustingetz.reg/email "dustin@example.com"] #_17592186046196)
                 ctx (context/element ctx 0)
                 ctx (extract (context/refocus+ ctx :dustingetz.reg/gender))]
             (context/data ctx))
           #:db{:ident :dustingetz.gender/male}))

    (is (= (for [[_ ctx] (context/spread-rows ctx)
                 [_ ctx] (context/spread-elements ctx)]
             (let [ctx (extract (context/refocus+ ctx :dustingetz.reg/gender))]
               (context/data ctx)))
           (for [[_ ctx] (context/spread-rows ctx)]
             ; infer element
             (let [ctx (extract (context/refocus+ ctx :dustingetz.reg/gender))]
               (context/data ctx)))
           [#:db{:ident :dustingetz.gender/male}
            #:db{:ident :dustingetz.gender/male}]))

    (is (= (let [ctx (context/row ctx [:dustingetz.reg/email "dustin@example.com"] #_17592186046196)
                 ctx (context/element ctx 0)
                 ctx (extract (context/refocus+ ctx :dustingetz.reg/gender))]
             @(:hypercrud.browser/eav ctx))
           (let [ctx (context/row ctx [:dustingetz.reg/email "dustin@example.com"] #_17592186046196)
                 ; infer element
                 ctx (extract (context/refocus+ ctx :dustingetz.reg/gender))]
             @(:hypercrud.browser/eav ctx))
           [[:dustingetz.reg/email "dustin@example.com"] :dustingetz.reg/gender :dustingetz.gender/male]))

    (is (= (for [[_ ctx] (context/spread-rows ctx)
                 [_ ctx] (context/spread-elements ctx)]
             (let [ctx (extract (context/refocus+ ctx :dustingetz.reg/gender))]
               @(:hypercrud.browser/eav ctx)))
           (for [[_ ctx] (context/spread-rows ctx)]
             ; infer elements
             (let [ctx (extract (context/refocus+ ctx :dustingetz.reg/gender))]
               @(:hypercrud.browser/eav ctx)))
           '([[:dustingetz.reg/email "dustin@example.com"] :dustingetz.reg/gender :dustingetz.gender/male]
              [[:dustingetz.reg/email "bob@example.com"] :dustingetz.reg/gender :dustingetz.gender/male]))))

  (testing "refocus to :ref :many from row")
  (testing "refocus from myself when already here but not at root")

  )

(deftest refocus-b
  (def ctx (mock-fiddle! :dustingetz/gender-shirtsize))
  (testing "refocus to :ref :one from top"
    (is (= (let [ctx (extract (context/refocus+ ctx :dustingetz.reg/gender))]
             ; Can't do it. Should this throw?
             (context/data ctx))
           nil))

    (is (= (for [[_ ctx] (context/spread-rows ctx)
                 [_ ctx] (context/spread-elements ctx)]
             (let [ctx (extract (context/refocus+ ctx :dustingetz.reg/gender))]
               (context/data ctx)))
           [#:db{:ident :dustingetz.gender/male}
            #:db{:ident :dustingetz.gender/male}]))

    (is (= (let [ctx (context/row ctx [:dustingetz.reg/email "dustin@example.com"] #_17592186046196)
                 ctx (context/element ctx 0)
                 ctx (extract (context/refocus+ ctx :dustingetz.reg/gender))]
             @(:hypercrud.browser/eav ctx))
           [[:dustingetz.reg/email "dustin@example.com"] :dustingetz.reg/gender :dustingetz.gender/male]))

    (is (= (for [[_ ctx] (context/spread-rows ctx)
                 [_ ctx] (context/spread-elements ctx)]
             (let [ctx (extract (context/refocus+ ctx :dustingetz.reg/gender))]
               @(:hypercrud.browser/eav ctx)))
           [[[:dustingetz.reg/email "dustin@example.com"] :dustingetz.reg/gender :dustingetz.gender/male]
            [[:dustingetz.reg/email "bob@example.com"] :dustingetz.reg/gender :dustingetz.gender/male]])))
  )

(deftest refocus-and-attribute-parallels
  (testing "refocus and attribute are nearly the same thing"
    (let [ctx (mock-fiddle! :tutorial.race/submission)]
      (let [ctx (extract (context/refocus+ ctx :dustingetz.reg/gender))]
        (is (= (context/smart-entity-identifier ctx (context/data ctx))
               :dustingetz.gender/male)))
      (let [ctx (context/attribute ctx :dustingetz.reg/gender)]
        (is (= (context/smart-entity-identifier ctx (context/data ctx))
               :dustingetz.gender/male)))))
  )

(deftest smart-identity
  (testing "smart-entity-identifier db/id"
    (let [ctx (mock-fiddle! :tutorial.race/submission)
          e (context/data ctx)]
      (is (= e
             {:db/id 17592186046196,
              :dustingetz.reg/email "dustin@example.com",
              :dustingetz.reg/name "Dustin Getz",
              :dustingetz.reg/age 102,
              :dustingetz.reg/birthdate #inst "2018-09-13T00:00:00.000-00:00",
              :dustingetz.reg/gender {:db/ident :dustingetz.gender/male},
              :dustingetz.reg/shirt-size {:db/ident :dustingetz.shirt-size/mens-large}}))
      (is (= (context/smart-entity-identifier ctx e)
             17592186046196))))

  (testing "smart-entity-identifier db/ident"
    (let [ctx (mock-fiddle! :tutorial.race/submission)]
      (let [ctx (extract (context/refocus+ ctx :dustingetz.reg/gender))]
        (is (= (context/smart-entity-identifier ctx (context/data ctx))
               :dustingetz.gender/male)))
      (let [ctx (extract (context/refocus+ ctx :dustingetz.reg/gender))]
        (is (= (context/smart-entity-identifier ctx (context/data ctx))
               :dustingetz.gender/male)))))

  (testing "smart-entity-identifier :db.unique/identity"
    (let [ctx (mock-fiddle! :dustingetz/slack-storm)
          [[e tx]] (context/data ctx)]
      (is (= e
             {:db/id 17592186047000,
              :dustingetz.post/title "is js/console.log syntax future proof?",
              :dustingetz.post/slug :asdf,
              :dustingetz.storm/channel "#clojurescript",
              :dustingetz.post/published-date #inst"2018-11-19T00:00:00.000-00:00"}))

      ; needs element to know schema, otherwise can't be done
      (is (= (context/smart-entity-identifier (context/element ctx 0) e) [:dustingetz.post/slug :asdf]))
      (is (= (context/smart-entity-identifier (context/element ctx 1) e) 17592186047000)) ; Untangle, this should probably assert
      (is (= (context/smart-entity-identifier ctx e) 17592186047000)) ; Untangle, this should probably assert
      ))
  )

(deftest formulas
  ; Todo smart-identity on :identity attrs

  (def ctx (mock-fiddle! :dustingetz/slack-storm))

  (testing ""
    (doall
      (for [[_ ctx] (context/spread-rows ctx)
            [_ ctx] (context/spread-elements ctx)
            #_#_[_ ctx] (context/spread-attributes ctx)]
        #_(context/data ctx)
        (context/eav ctx)
        )))

  #_(testing "smart identity should prefer identity attrs to db/id"
      (is (= (for [[_ ctx] (context/spread-rows ctx)
                   [_ ctx] (context/spread-elements ctx)
                   #_#_[_ ctx] (context/spread-attributes ctx)]
               (context/eav ctx))
             [[nil :dustingetz/slack-storm 17592186047000]
              [nil :dustingetz/slack-storm 13194139535895]  ; tx
              [nil :dustingetz/slack-storm 17592186047105]
              [nil :dustingetz/slack-storm 13194139536000]  ; tx
              ]))
      )

  #_(testing "FindRel fiddle-level, how to identify each rel?"
      (is (= (for [[_ ctx] (context/spread-rows ctx)
                   [_ ctx] (context/spread-elements ctx)
                   #_#_[_ ctx] (context/spread-attributes ctx)]
               (context/eav ctx))

             ; I think these shouldn't be addressable at all from fiddle-level.
             [[nil :dustingetz/slack-storm [:dustingetz.post/slug :asdf]]
              [nil :dustingetz/slack-storm 13194139535895]  ; (max ?tx)
              [nil :dustingetz/slack-storm 17592186047105]  ; ?e but :identity attr is nil
              [nil :dustingetz/slack-storm 13194139536000]  ; (max ?tx)
              ]))
      )

  (testing "FindRel tuple at element level"
    (testing "addressing rowtuple by tupled rowkey"
      (is (= (let [ctx (mock-fiddle! :dustingetz/slack-storm)]
               (for [ctx [(context/row ctx [[:dustingetz.post/slug :asdf] 13194139535895])]]
                 (context/data ctx)))
             [[{:db/id 17592186047000,
                :dustingetz.post/title "is js/console.log syntax future proof?",
                :dustingetz.post/slug :asdf,
                :dustingetz.storm/channel "#clojurescript",
                :dustingetz.post/published-date #inst"2018-11-19T00:00:00.000-00:00"}
               13194139535895]])))

    (testing "extract tuples"
      (is (= (let [ctx (mock-fiddle! :dustingetz/slack-storm)]
               (for [[_ ctx] (context/spread-rows ctx)]
                 (context/data ctx)))
             [[{:db/id 17592186047000,
                :dustingetz.post/title "is js/console.log syntax future proof?",
                :dustingetz.post/slug :asdf,
                :dustingetz.storm/channel "#clojurescript",
                :dustingetz.post/published-date #inst"2018-11-19T00:00:00.000-00:00"}
               13194139535895]
              [{:db/id 17592186047105,
                :dustingetz.post/title "large strings and high churn attrs blow out indexes",
                :dustingetz.storm/channel "#datomic",
                :dustingetz.post/published-date #inst"2018-11-21T00:00:00.000-00:00"}
               13194139536000]])))

    (testing "flatten to elements"
      (is (= (let [ctx (mock-fiddle! :dustingetz/slack-storm)]
               (for [[_ ctx] (context/spread-rows ctx)
                     [_ ctx] (context/spread-elements ctx)]
                 (context/data ctx)))
             [{:db/id 17592186047000,
               :dustingetz.post/title "is js/console.log syntax future proof?",
               :dustingetz.post/slug :asdf,
               :dustingetz.storm/channel "#clojurescript",
               :dustingetz.post/published-date #inst"2018-11-19T00:00:00.000-00:00"}
              13194139535895
              {:db/id 17592186047105,
               :dustingetz.post/title "large strings and high churn attrs blow out indexes",
               :dustingetz.storm/channel "#datomic",
               :dustingetz.post/published-date #inst"2018-11-21T00:00:00.000-00:00"}
              13194139536000])))

    (testing "rowkey then all elements"
      (is (= (let [ctx (mock-fiddle! :dustingetz/slack-storm)]
               (for [ctx [(context/row ctx [[:dustingetz.post/slug :asdf] 13194139535895])]
                     [_ ctx] (context/spread-elements ctx)]
                 (context/data ctx)))
             [{:db/id 17592186047000,
               :dustingetz.post/title "is js/console.log syntax future proof?",
               :dustingetz.post/slug :asdf,
               :dustingetz.storm/channel "#clojurescript",
               :dustingetz.post/published-date #inst"2018-11-19T00:00:00.000-00:00"}
              13194139535895])))

    (testing "rowkey then single elements"
      (is (let [ctx (mock-fiddle! :dustingetz/slack-storm)
                ctx (context/row ctx [[:dustingetz.post/slug :asdf] 13194139535895])]
            (is (= (context/eav ctx) [nil :dustingetz/slack-storm nil])) ; could potentially set v to the rowkey here
            (is (= (context/data (context/element ctx 0))
                   {:db/id 17592186047000,
                    :dustingetz.post/title "is js/console.log syntax future proof?",
                    :dustingetz.post/slug :asdf,
                    :dustingetz.storm/channel "#clojurescript",
                    :dustingetz.post/published-date #inst "2018-11-19T00:00:00.000-00:00"}))
            (is (= (context/data (context/element ctx 1)) 13194139535895)))))

    (testing "element level does set value, it is not ambigous due to set semantics in FindRel"
      (is (let [ctx (mock-fiddle! :dustingetz/slack-storm)
                ctx (context/row ctx [[:dustingetz.post/slug :asdf] 13194139535895])]
            (is (= (context/eav ctx) [nil :dustingetz/slack-storm nil])) ; could potentially set v to the rowkey here
            ; Originally thought:
            ; Can't set v to [:dustingetz.post/slug :asdf] because A is ambiguous.
            ; Can't set v to 5895 because A is ambiguous.
            ; NO, wrong.
            ; Arguably this is NOT ambiguous due to set semantics. The same entity can't be pulled twice in a find relation.
            (is (= (context/eav (context/element ctx 0)) [nil :dustingetz/slack-storm [:dustingetz.post/slug :asdf]]))
            (is (= (context/eav (context/element ctx 1)) [nil :dustingetz/slack-storm 13194139535895])))))

    (testing "key all the way and then attributes"
      (let [ctx (mock-fiddle! :dustingetz/slack-storm)
            ctx (context/row ctx [[:dustingetz.post/slug :asdf] 13194139535895])
            ctx (context/element ctx 0)]
        (is (= (context/eav ctx) [nil :dustingetz/slack-storm [:dustingetz.post/slug :asdf]]))
        (is (= (context/data ctx)
               {:db/id 17592186047000,
                :dustingetz.post/title "is js/console.log syntax future proof?",
                :dustingetz.post/slug :asdf,
                :dustingetz.storm/channel "#clojurescript",
                :dustingetz.post/published-date #inst "2018-11-19T00:00:00.000-00:00"}))
        (let [ctx (context/attribute ctx :dustingetz.post/title)]
          (is (= (context/data ctx) "is js/console.log syntax future proof?"))
          (is (= (context/eav ctx) [[:dustingetz.post/slug :asdf] :dustingetz.post/title "is js/console.log syntax future proof?"])))

        (is (= (for [[_ ctx] (context/spread-attributes ctx)]
                 (context/eav ctx))
               [[[:dustingetz.post/slug :asdf] :db/id 17592186047000]
                [[:dustingetz.post/slug :asdf] :dustingetz.post/title "is js/console.log syntax future proof?"]
                [[:dustingetz.post/slug :asdf] :dustingetz.post/slug :asdf]
                [[:dustingetz.post/slug :asdf] :dustingetz.storm/channel "#clojurescript"]
                [[:dustingetz.post/slug :asdf] :dustingetz.post/published-date #inst"2018-11-19T00:00:00.000-00:00"]]))))
    )

  (testing "FindRel tuple at attr level")

  (testing "FindRel attr-level, how to identify each rel?"

    (is (= (let [ctx (mock-fiddle! :dustingetz/slack-storm)]
             (for [#_#_[_ ctx] (context/spread-rows ctx)
                   ctx [(context/row ctx [[:dustingetz.post/slug :asdf] 13194139535895])]
                   [_ ctx] (context/spread-elements ctx)
                   #_#_[_ ctx] (context/spread-attributes ctx)]
               (context/eav ctx)))
           [[nil :dustingetz/slack-storm [:dustingetz.post/slug :asdf]]
            [nil :dustingetz/slack-storm 13194139535895]]))

    (is (= (let [ctx (mock-fiddle! :dustingetz/slack-storm)]
             (for [ctx [(context/row ctx [[:dustingetz.post/slug :asdf] 13194139535895])]
                   [_ ctx] (context/spread-elements ctx)
                   [_ ctx] (context/spread-attributes ctx)]
               (context/eav ctx)))
           [[[:dustingetz.post/slug :asdf] :db/id 17592186047000]
            [[:dustingetz.post/slug :asdf] :dustingetz.post/title "is js/console.log syntax future proof?"]
            [[:dustingetz.post/slug :asdf] :dustingetz.post/slug :asdf]
            [[:dustingetz.post/slug :asdf] :dustingetz.storm/channel "#clojurescript"]
            [[:dustingetz.post/slug :asdf] :dustingetz.post/published-date #inst"2018-11-19T00:00:00.000-00:00"]]))

    (is (= (let [ctx (mock-fiddle! :dustingetz/slack-storm)]
             (for [ctx [(context/row ctx [[:dustingetz.post/slug :asdf] 13194139535895])]
                   [_ ctx] (context/spread-elements ctx)
                   [_ ctx] (context/spread-attributes ctx)]
               (context/eav ctx)))
           [[[:dustingetz.post/slug :asdf] :db/id 17592186047000]
            [[:dustingetz.post/slug :asdf] :dustingetz.post/title "is js/console.log syntax future proof?"]
            [[:dustingetz.post/slug :asdf] :dustingetz.post/slug :asdf]
            [[:dustingetz.post/slug :asdf] :dustingetz.storm/channel "#clojurescript"]
            [[:dustingetz.post/slug :asdf] :dustingetz.post/published-date #inst"2018-11-19T00:00:00.000-00:00"]]))

    (is (= (let [ctx (mock-fiddle! :dustingetz/slack-storm)]
             (for [[_ ctx] (context/spread-rows ctx)
                   [_ ctx] (context/spread-elements ctx)
                   [_ ctx] (context/spread-attributes ctx)]
               (context/eav ctx)))
           [[[:dustingetz.post/slug :asdf] :db/id 17592186047000]
            [[:dustingetz.post/slug :asdf] :dustingetz.post/title "is js/console.log syntax future proof?"]
            [[:dustingetz.post/slug :asdf] :dustingetz.post/slug :asdf]
            [[:dustingetz.post/slug :asdf] :dustingetz.storm/channel "#clojurescript"]
            [[:dustingetz.post/slug :asdf] :dustingetz.post/published-date #inst"2018-11-19T00:00:00.000-00:00"]
            [17592186047105 :db/id 17592186047105]
            [17592186047105 :dustingetz.post/title "large strings and high churn attrs blow out indexes"]
            [17592186047105 :dustingetz.post/slug nil]
            [17592186047105 :dustingetz.storm/channel "#datomic"]
            [17592186047105 :dustingetz.post/published-date #inst"2018-11-21T00:00:00.000-00:00"]]))
    )

  (testing "identity focusing"
    (def ctx (mock-fiddle! :dustingetz.tutorial/blog))
    (is (= (context/data ctx)
           [[{:db/id 17592186047105,
              :dustingetz.post/published-date #inst"2018-11-21T00:00:00.000-00:00",
              :dustingetz.post/title "large strings and high churn attrs blow out indexes",
              :dustingetz.post/slug :large-strings-and-high-churn-attrs-blow-out-indexes}]
            [{:db/id 17592186047142,
              :dustingetz.post/published-date #inst"2018-11-22T15:57:34.277-00:00",
              :dustingetz.post/title "automatic CRUD links",
              :dustingetz.post/slug :automatic-CRUD-links}]]))
    (is (= (for [[_ ctx] (context/spread-elements ctx)]
             (context/eav ctx))
           [[nil :dustingetz.tutorial/blog nil]]))

    ;(is (= (for [[_ ctx] (context/spread-elements ctx)]
    ;         ; Asking for data without a row and unable to infer row
    ;         (context/data ctx))
    ;       nil))

    (is (= (for [[_ ctx] (context/spread-rows ctx)
                 [_ ctx] (context/spread-elements ctx)]
             (context/data ctx))
           [{:db/id 17592186047105,
             :dustingetz.post/published-date #inst"2018-11-21T00:00:00.000-00:00",
             :dustingetz.post/title "large strings and high churn attrs blow out indexes",
             :dustingetz.post/slug :large-strings-and-high-churn-attrs-blow-out-indexes}
            {:db/id 17592186047142,
             :dustingetz.post/published-date #inst"2018-11-22T15:57:34.277-00:00",
             :dustingetz.post/title "automatic CRUD links",
             :dustingetz.post/slug :automatic-CRUD-links}]))

    (is (= (for [[_ ctx] (context/spread-rows ctx)
                 [_ ctx] (context/spread-elements ctx)]
             (context/eav ctx))
           [[nil :dustingetz.tutorial/blog [:dustingetz.post/slug :large-strings-and-high-churn-attrs-blow-out-indexes]]
            [nil :dustingetz.tutorial/blog [:dustingetz.post/slug :automatic-CRUD-links]]]))

    (is (= (for [[_ ctx] (context/spread-rows ctx)
                 [_ ctx] (context/spread-elements ctx)]
             (context/eav ctx))
           [[nil :dustingetz.tutorial/blog [:dustingetz.post/slug :large-strings-and-high-churn-attrs-blow-out-indexes]]
            [nil :dustingetz.tutorial/blog [:dustingetz.post/slug :automatic-CRUD-links]]]))

    (is (= (hypercrud.browser.context/row-key ctx [{:db/id 17592186047142,
                                                    :dustingetz.post/published-date #inst"2018-11-22T15:57:34.277-00:00",
                                                    :dustingetz.post/title "automatic CRUD links",
                                                    :dustingetz.post/slug :automatic-CRUD-links}])
           ; Wrapper tuple. This kind of makes row-keys confusing for userland to figure out, they have to call
           ; the key-row api.
           [[:dustingetz.post/slug :automatic-CRUD-links]]))

    (def ctx (-> (mock-fiddle! :dustingetz.tutorial/blog)
                 (context/row [[:dustingetz.post/slug :automatic-CRUD-links]])
                 (context/element 0)))
    (is (= (:hypercrud.browser/result-path ctx) [[[:dustingetz.post/slug :automatic-CRUD-links]] 0]))
    (is (= (keys @(:hypercrud.browser/result-index ctx))
           [[[:dustingetz.post/slug :large-strings-and-high-churn-attrs-blow-out-indexes]]
            [[:dustingetz.post/slug :automatic-CRUD-links]]]))

    (is (= (context/data ctx)
           {:db/id 17592186047142,
            :dustingetz.post/published-date #inst"2018-11-22T15:57:34.277-00:00",
            :dustingetz.post/title "automatic CRUD links",
            :dustingetz.post/slug :automatic-CRUD-links}))
    (is (= (context/eav ctx)
           [nil :dustingetz.tutorial/blog [:dustingetz.post/slug :automatic-CRUD-links]]))

    #_(let [#_#_ctx (context/element ctx 0)]
        (is (= (context/eav ctx) [nil :dustingetz/slack-storm [:dustingetz.post/slug :asdf]]))
        (is (= (context/data ctx)
               {:db/id 17592186047000,
                :dustingetz.post/title "is js/console.log syntax future proof?",
                :dustingetz.post/slug :asdf,
                :dustingetz.storm/channel "#clojurescript",
                :dustingetz.post/published-date #inst "2018-11-19T00:00:00.000-00:00"}))
        (let [ctx (context/attribute ctx :dustingetz.post/title)]
          (is (= (context/data ctx) "is js/console.log syntax future proof?"))
          (is (= (context/eav ctx) [[:dustingetz.post/slug :asdf] :dustingetz.post/title "is js/console.log syntax future proof?"])))

        (is (= (for [[_ ctx] (context/spread-attributes ctx)]
                 (context/eav ctx))
               [[[:dustingetz.post/slug :asdf] :db/id 17592186047000]
                [[:dustingetz.post/slug :asdf] :dustingetz.post/title "is js/console.log syntax future proof?"]
                [[:dustingetz.post/slug :asdf] :dustingetz.post/slug :asdf]
                [[:dustingetz.post/slug :asdf] :dustingetz.storm/channel "#clojurescript"]
                [[:dustingetz.post/slug :asdf] :dustingetz.post/published-date #inst"2018-11-19T00:00:00.000-00:00"]])))

    )

  (testing "link primitives"
    (def ctx (-> (mock-fiddle! :dustingetz.tutorial/blog)
                 (context/row [[:dustingetz.post/slug :automatic-CRUD-links]])
                 (context/element 0)
                 (context/attribute :dustingetz.post/slug)))
    (is (= (->> (hyperfiddle.data/spread-links-here ctx) (map first))
           [17592186047370 17592186047372]))
    (is (= (->> (hyperfiddle.data/spread-links-here ctx) (map (comp :link/path deref second)))
           [":dustingetz.post/slug" ":dustingetz.post/slug"]))
    (is (= (->> (hyperfiddle.data/spread-links-here ctx :hf/new) (map (comp :link/path deref second)))
           [":dustingetz.post/slug"]))
    (is (= (->> (hyperfiddle.data/spread-links-here ctx :dustingetz.tutorial/view-post)
                (map (comp :link/path deref second)))
           [":dustingetz.post/slug"]))

    (is (= (mlet [r-link (hyperfiddle.data/select-here+ ctx :hf/new1)]
             (return 42))
           (left "no match for class: :hf/new1")))

    (is (= (->> (hyperfiddle.data/select-here+ ctx :hf/new) (unwrap (constantly nil)) deref)
           {:db/id 17592186047372,
            :link/class [:hf/new],
            :link/fiddle {:db/id 17592186047373,
                          :fiddle/ident :dustingetz.tutorial.blog/new-post,
                          :fiddle/type :entity,
                          :fiddle/pull "[:db/id *]",
                          :fiddle/pull-database "$",
                          :fiddle/markdown "### :dustingetz.tutorial.blog/new-post",
                          :fiddle/renderer nil},
            :link/path ":dustingetz.post/slug",
            :link/rel :hf/new,
            :link/tx-fn ":user/new-post",
            :link/formula "(constantly (hyperfiddle.api/tempid! ctx))"})))

  (testing ":identity link refocus v is lookup-ref"
    (def ctx (-> (mock-fiddle! :dustingetz.tutorial/blog)
                 (context/row [[:dustingetz.post/slug :automatic-CRUD-links]])
                 (context/element 0)
                 (context/attribute :dustingetz.post/slug)))
    (def r-link (->> (hyperfiddle.data/select-here+ ctx :dustingetz.tutorial/view-post) (unwrap (constantly nil))))
    (is (= (mlet [[ctx ?route] (context/refocus-to-link+ ctx @r-link)]
             (context/eav ctx))
           [nil :dustingetz.tutorial/blog [:dustingetz.post/slug :automatic-CRUD-links]])))

  (testing ":identity :hf/new formula evaluates to tempid"
    (def ctx (-> (mock-fiddle! :dustingetz.tutorial/blog)   ; FindRel
                 (context/row [[:dustingetz.post/slug :automatic-CRUD-links]])
                 (context/element 0)                        ; required for FindRel
                 (context/attribute :dustingetz.post/slug)))
    (def link @(extract (hyperfiddle.data/select-here+ ctx :hf/new)))
    (testing "at fiddle level, link :hf/new eav does not have a parent"
      (is (= (mlet [[ctx r+route] (context/refocus-to-link+ ctx link)]
               (context/eav ctx))
             ; should it be [nil nil "479925454"] from the txfn perspective?
             [nil :dustingetz.tutorial/blog "hyperfiddle.tempid--2017569654"]))
      (is (= (mlet [[ctx +route] (context/refocus-to-link+ ctx link)]
               (return +route))
             (right [:dustingetz.tutorial.blog/new-post [#entity["$" "hyperfiddle.tempid--2017569654"]]]))))
    )

  (testing "iframe at double nested attr"
    (def ctx (-> (mock-fiddle! :dustingetz.seattle/communities)
                 (context/row 17592186045537)
                 (context/attribute :community/neighborhood)
                 (context/attribute :neighborhood/district)
                 (context/attribute :district/region)))
    (is (= (context/eav ctx) [[:district/name "Ballard"] :district/region :region/nw]))
    (is @(->> (hyperfiddle.data/select-here+ ctx :hf/iframe) (unwrap (constantly nil))))
    ; Really we want to assert it was hydrated but no tests for that yet
    )
  )

#_(testing "refocus link from tupled qfind, identity focused from element ctx"
    (def ctx (-> (mock-fiddle! :dustingetz.tutorial/blog)
                 (context/row [[:dustingetz.post/slug :automatic-CRUD-links]])
                 #_(context/element 0)
                 (context/attribute :dustingetz.post/slug)))
    (def r-link (->> (hyperfiddle.data/select-here+ ctx :hf/new) (unwrap (constantly nil))))
    (testing "at fiddle level, link :hf/new eav does not have a parent"
      (is (= (let [[ctx r+route] (context/refocus-to-link+ ctx r-link)]
               (context/eav ctx))
             ; should it be [nil nil "479925454"] from the txfn perspective?
             [nil :dustingetz.tutorial/blog "479925454"])))
    )

(testing "refocus link from tupled qfind, identity focused from result ctx"
  ; Select a link from the root context that is reachable but not exactly here.
  ; So select post/slug from a row ctx, as can happen in a view but not an autogrid.
  ; This exercises tag-v-with-color edge cases
  (def ctx (-> (mock-fiddle! :dustingetz.tutorial/blog)     ; FindRel-1
               (context/row [[:dustingetz.post/slug :automatic-CRUD-links]])
               #_(context/element 0)                        ; Specifically no element
               ))
  (def link @(hyperfiddle.data/select ctx :hf/new))
  (is (= (mlet [[ctx route] (context/refocus-to-link+ ctx link)]
           (context/eav ctx))
         [nil :dustingetz.tutorial/blog "hyperfiddle.tempid--2017569654"]))
  (is (= (mlet [[ctx route] (context/refocus-to-link+ ctx link)]
           (return route))
         ; This works because refocus hardcodes element 0, which it turns out is almost always
         ; what the custom renderer wants.
         (right [:dustingetz.tutorial.blog/new-post [#entity["$" "hyperfiddle.tempid--2017569654"]]])))
  )

(deftest schema-bug-part-1
  (testing "works"
    (def ctx (mock-fiddle! :dustingetz.test/schema-ident-findcoll))
    (is (= (let [ctx (context/row ctx :db.sys/partiallyIndexed)]
             (context/data ctx))
           #:db{:ident :db.sys/partiallyIndexed}))
    (is (= (let [ctx (context/row ctx :db.sys/partiallyIndexed)]
             (context/eav ctx))
           [nil :seattle/neighborhoods :db.sys/partiallyIndexed]))
    (is (= (let [ctx (context/row ctx :db.sys/partiallyIndexed)]
             (context/row-key ctx (context/data ctx)))
           :db.sys/partiallyIndexed))
    (is (= (let [ctx (context/row ctx :db.sys/partiallyIndexed)]
             (context/row-key-v ctx (context/data ctx)))
           :db.sys/partiallyIndexed))

    (is (= (let [ctx (context/row ctx :db.sys/partiallyIndexed)
                 ctx (context/element ctx 0)]
             (context/eav ctx))
           [nil :seattle/neighborhoods :db.sys/partiallyIndexed]))
    (is (= (let [ctx (context/row ctx :db.sys/partiallyIndexed)
                 ctx (context/element ctx 0)]
             (context/data ctx))
           #:db{:ident :db.sys/partiallyIndexed}))

    (is (= (count (for [[_ ctx] (context/spread-rows ctx)]
                    nil))
           99))))

(deftest findcoll-ident-card-many
  (is (-> (let [ctx (mock-fiddle! :dustingetz.test/findcoll-ident-cardinality-many)]
            (for [[_ ctx] (context/spread-rows ctx)]
              (context/data ctx)))
          count (> 0)))

  )

;(deftest schema-but-part-2
;  (testing "Crashes on the fifth row which is :db.install/valueType"
;    (let [ctx (mock-fiddle! :dustingetz.test/schema-ident-findcoll)]
;      (->> (for [[_ ctx] (drop 1 (context/spread-rows ctx))]
;             (try
;               (context/data ctx)
;               [(:hypercrud.browser/result-path ctx)
;                nil
;                #_@(r/cursor (:hypercrud.browser/result-index ctx) (:hypercrud.browser/result-path ctx))]
;               (catch Exception e
;                 [
;                  (:hypercrud.browser/result-path ctx)
;                  e
;                  #_@(r/cursor (:hypercrud.browser/result-index ctx) (:hypercrud.browser/result-path ctx))
;                  ])))
;           (filter second)
;           doall))
;    )
;
;  (testing "I dont know why this crashes"
;    (def ctx (mock-fiddle! :dustingetz.test/schema-ident-findcoll))
;    (is (= (let [ctx (context/row ctx :db.install/valueType)]
;             (context/data ctx))
;           #:db{:ident :db.sys/partiallyIndexed}))
;    (is (= (let [ctx (context/row ctx :db.install/valueType)]
;             (context/eav ctx))
;           [nil :seattle/neighborhoods :db.sys/partiallyIndexed]))
;    (is (= (let [ctx (context/row ctx :db.install/valueType)]
;             (context/row-key ctx (context/data ctx)))
;           :db.install/valueType))
;    (is (= (let [ctx (context/row ctx :db.install/valueType)]
;             (context/row-key-v ctx (context/data ctx)))
;           :db.install/valueType))
;
;    (for [[_ ctx] (context/spread-rows ctx)]
;      (context/eav ctx))
;
;
;
;    (for [[_ ctx] (context/spread-rows ctx)
;          [_ ctx] (context/spread-elements ctx)]
;      (context/data ctx))
;
;    ))

(deftest links111
  ;(testing "refocus link from scalar qfind, identity focused from result ctx (infer element)"
  ;  (is true))
  ;(testing "refocus link from scalar qfind, identity focused from element ctx"
  ;  (is true))

  (testing "race shirt-sizes iframe"
    (let [ctx (mock-fiddle! :tutorial.race/submission)]
      (is (-> @(hyperfiddle.data/select-many ctx :dustingetz.reg/gender)
              first :link/fiddle :fiddle/ident (= :tutorial.race/shirt-sizes)))
      ;(println @(hyperfiddle.data/select-many ctx :tutorial.race/submission))
      (is (= 2 (count @(hyperfiddle.data/select-many ctx :hf/iframe)))))
    )

  (testing "seattle neighborhood districts iframe"
    (def ctx (mock-fiddle! :seattle/neighborhoods))
    (is (-> @(hyperfiddle.data/select-many-here ctx :seattle/districts)
            first :link/fiddle :fiddle/ident (= :seattle/districts)))

    #_(testing "if head-sentinel, no v"
        (let [ctx (assoc ctx :hypercrud.browser/head-sentinel true)
              ctx (context/row ctx 17592186045522)
              ctx (context/attribute ctx :neighborhood/district)]
          (is (= (context/eav ctx) [nil :neighborhood/district nil])))))

  (testing "indexed-links"

    (def ctx (mock-fiddle! :seattle/neighborhoods))
    (is @(:hypercrud.browser/link-index ctx))
    (is @(context/links-at (:hypercrud.browser/link-index ctx) #{}))
    (is @(context/links-in-dimension-r ctx #{}))
    )
  )

(deftest schema-aliases
  (testing "Disabled test breaks without schema alias support"
    (def ctx (mock-fiddle! :cookbook/markdown-table))
    (is (= (context/data ctx)
           [{:db/id 17592186046744, :task/title "Feed baby", :task/completed true}
            {:db/id 17592186046745, :task/title "Mow the lawn"}
            {:db/id 17592186046746, :task/title "Do the dishes", :task/completed true}]))

    (def ctx (-> (mock-fiddle! :cookbook/markdown-table)    ; FindColl
                 (context/row 17592186046744)
                 ;(context/element 0)                        ; required for FindRel
                 #_(context/attribute :dustingetz.post/slug)))
    (is (= (context/data ctx)
           {:db/id 17592186046744, :task/title "Feed baby", :task/completed true}))
    (is (= (context/eav ctx)
           [nil :cookbook/markdown-table 17592186046744]))
    #_(is (= (for [ctx [(context/row ctx 17592186046744)]
                   [_ ctx] (context/spread-elements ctx)
                   [_ ctx] (context/spread-attributes ctx)]
               (context/eav ctx))
             [[nil :db/id nil]
              [nil :cookbook/markdown-table nil]
              [nil :cookbook/markdown-table nil]]))
    ))

(deftest ide-domain-databases
  (testing "yaaa"
    (def ctx (mock-fiddle! fixtures.domains/schemas
                           fixtures.domains/fiddles
                           :hyperfiddle.ide/domain))
    (def +ctx (context/refocus+ ctx :domain/databases))
    (is (left? +ctx))
    #_(= (context/eav ctx) [17592186046196 :dustingetz.reg/age 102]))
  )

(deftest txfn
  []

  (testing "counter button at scalar"
    (def ctx (mock-fiddle! :dustingetz/counter))
    (def ctx (extract (context/refocus+ ctx :dustingetz.reg/age)))
    (= (context/eav ctx) [17592186046196 :dustingetz.reg/age 102])

    )

  ; hf/new at fiddle level
  ; How does it pick the color and set the right eav for [:db/add e a v]
  ; v is formula. What is e?
  ; There isn't, this adds parent-child rel whichj we don't have! Why are we here.

  )

#_(deftest deps-satisfied-1
    []
    (is (= (deps-satisfied? [0 :fiddle/links] [0 :fiddle/links :link/fiddle]) false))
    (is (= (deps-satisfied? [0 :gender] [0 :shirt-size]) true))
    (is (= (deps-satisfied? [0] [0 :shirt-size]) true))
    (is (= (deps-satisfied? [0] [0 :fiddle/links :link/fiddle]) false))
    (is (= (deps-satisfied? [0 :fiddle/links] [0 :fiddle/links :link/fiddle]) false))
    (is (= (deps-satisfied? [] [0]) false))
    (is (= (deps-satisfied? [] [0 :user/user-id]) false))   ; This becomes true if a relation can be implied in scope
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
