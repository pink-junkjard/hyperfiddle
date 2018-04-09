(ns contrib.reactive-test
  (:require
    [clojure.test :refer [deftest is]]
    [contrib.reactive :as r]))


(deftest test-constantly []
  #?(:cljs                                                  ; clj implementation not yet implemented
     (is (= (r/constantly 1) (r/constantly 1))))

  (is (= 1 ((r/constantly 1) "asdf"))))
