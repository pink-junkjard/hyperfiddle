(ns contrib.macros-test
  (:require
    [clojure.test :refer [deftest is]]
    [contrib.macros :refer [cond-let]]))


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
