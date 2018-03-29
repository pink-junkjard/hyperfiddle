(ns hypercrud.util.core-test
  (:require [#?(:clj clojure.test :cljs cljs.test)
             #?(:clj :refer :cljs :refer-macros) [deftest is]]
            [hypercrud.util.core :refer [split-first rtrim-coll abc]]))



(def s "0/1/2?3?4#5#6")
(deftest split-first-1
  []
  (is (= (split-first s "/") ["0" "1/2?3?4#5#6"]))
  (is (= (split-first s "?") ["0/1/2" "3?4#5#6"]))
  (is (= (split-first s "#") ["0/1/2?3?4" "5#6"])))


(deftest seq-rtrim-1
  []
  (is (= (rtrim-coll nil? [:post nil]) [:post])))

(deftest abc-1
  []
  (is (= (take 4 (abc)) '(:a :b :c :d)))
  (is (= (count (doall (abc))) 26))
  (is (= (last (abc)) :z))
  )
