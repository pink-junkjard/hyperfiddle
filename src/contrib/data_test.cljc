(ns contrib.data-test
  (:require [clojure.test :refer [deftest is]]
            [contrib.data :refer [cond-let pad map-pad rtrim-coll]]))


(comment
  (map + [1 1 1] [1 1 1 1])                                 ;=> (2 2 2)
  ((map-pad 0) + [1 1 1] [1 1 1 1])                         ;=> (2 2 2 1)
  )

(deftest pad-1
  []
  (is (= ((map-pad 0) + [1 1 1] [1 1 1 1]) '(2 2 2 1)))
  )

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
