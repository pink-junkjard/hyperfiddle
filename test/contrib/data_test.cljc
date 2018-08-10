(ns contrib.data-test
  (:require [clojure.test :refer [deftest is]]
            [contrib.data :refer [cond-let map-pad pad rtrim-coll fix-arity fvor take-to
                                  compare-by-index ancestry-common]]))


(comment
  (map + [1 1 1] [1 1 1 1])                                 ;=> (2 2 2)
  ((map-pad 0) + [1 1 1] [1 1 1 1])                         ;=> (2 2 2 1)
  )

(deftest pad-1
  []
  (is (= ((map-pad 0) + [1 1 1] [1 1 1 1]) '(2 2 2 1)))
  )

(deftest test-take-to []
  (is (= (take-to #(not= % 4) (range 10))
         (range 5)))

  (is (= (take-to #{:a :b} [:a :b :a :c :a :b :a])
         (list :a :b :a :c))))

(deftest test-cond-let []
  (is (= 2 (cond-let
             [a 1] (inc a)
             [a 3] "it's 3!")))

  (is (= "it's 3!"
         (cond-let
           [a nil] (inc a)
           [b 3] "it's 3!")))

  (is (nil? (cond-let
              [a nil] (inc a)))))

(deftest seq-rtrim-1
  []
  (is (= (rtrim-coll nil? [:post nil]) [:post]))
  (is (= (rtrim-coll nil? [:post nil nil "frag"]) [:post nil nil "frag"]))
  )

(deftest fix-arity-1 []
  (is (= ((fix-arity (fnil inc 0) 1) 42) 43))
  (is (= ((fix-arity (fnil inc 0) 1) nil) 1))
  (is (= ((fix-arity (fnil inc 0) 1) nil :a) 1)))

(deftest fvor-1 []
  (is (= ((fvor #(.toLocaleDateString %) "–") nil) "–")))

(def ordering [:fiddle/ident :fiddle/type :fiddle/pull-database :fiddle/pull :fiddle/query
               :fiddle/renderer :fiddle/css :fiddle/markdown :fiddle/links :fiddle/hydrate-result-as-fiddle])
(deftest compare-by-index-
  []
  (is (= (apply sorted-set-by
                (compare-by-index ordering)
                [:fiddle/renderer :fiddle/ident :fiddle/css :fiddle/markdown :fiddle/type])
         (sorted-set :fiddle/ident :fiddle/type :fiddle/renderer :fiddle/css :fiddle/markdown))))

(deftest ancestry-common-1
  []
  (is (= (ancestry-common [1 2 3 4 5 6] [1 2 3 4 10 11])
         '(1 2 3 4))))
