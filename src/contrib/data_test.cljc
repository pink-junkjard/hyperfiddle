(ns contrib.data-test
  (:require [clojure.test :refer [deftest is]]
            [contrib.data :refer [pad map-pad]]))


(comment
  (map + [1 1 1] [1 1 1 1])                                 ;=> (2 2 2)
  ((map-pad 0) + [1 1 1] [1 1 1 1])                         ;=> (2 2 2 1)
  )

(deftest pad-1
  []
  (is (= ((map-pad 0) + [1 1 1] [1 1 1 1]) '(2 2 2 1)))
  )