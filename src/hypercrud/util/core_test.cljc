(ns hypercrud.util.core-test
  (:require [#?(:clj clojure.test :cljs cljs.test)
             #?(:clj :refer :cljs :refer-macros) [deftest is]]
            [hypercrud.util.core :refer [split-first]]))



(def s "0/1/2?3?4#5#6")
(deftest split-first-1
  []
  (is (= (split-first s "/") ["0" "1/2?3?4#5#6"]))
  (is (= (split-first s "?") ["0/1/2" "3?4#5#6"]))
  (is (= (split-first s "#") ["0/1/2?3?4" "5#6"])))
