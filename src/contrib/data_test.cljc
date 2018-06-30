(ns contrib.data-test
  (:require [clojure.test :refer [deftest is]]
            [contrib.data :refer [cond-let map-pad pad rtrim-coll fix-arity fvor take-to]]))


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
