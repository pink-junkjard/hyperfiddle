(ns contrib.reactive-test
  (:require
    [clojure.test :refer [deftest is]]
    [contrib.reactive :as r]))


(deftest test-constantly []
  #?(:cljs                                                  ; clj implementation not yet implemented
     (is (= (r/constantly 1) (r/constantly 1))))

  (is (= 1 ((r/constantly 1) "asdf"))))

(deftest test-fapply []
  (let [reactive-inc (r/atom inc)]
    (is (= 2 @(r/fapply reactive-inc (r/atom 1)))))

  (let [reactive-inc (r/atom (fn [a] (fn [b] (+ a b))))]
    (is (= 3 @(r/fapply reactive-inc (r/atom 1) (r/atom 2))))))
