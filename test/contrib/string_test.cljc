(ns contrib.string-test
  (:require [clojure.pprint]
            [clojure.test :refer [deftest is]]
            [contrib.string :refer [abc empty->nil blank->nil split-first]]))


(deftest empty->nil-1
  (is (= (empty->nil nil) nil))
  (is (= (empty->nil "") nil))
  (is (= (empty->nil " ") " "))
  (is (= (empty->nil "a") "a")))

(deftest blank->nil-1
  (is (= (blank->nil nil) nil))
  (is (= (blank->nil "") nil))
  (is (= (blank->nil " ") nil))
  (is (= (blank->nil "      ") nil))
  (is (not= (blank->nil "a") nil))
  (is (not= (blank->nil "   a") nil))
  (is (not= (blank->nil "   a   ") nil)))

(def s "0/1/2?3?4#5#6")
(deftest split-first-1
  []
  (is (= (split-first s #"/") ["0" "1/2?3?4#5#6"]))
  (is (= (split-first s #"\?") ["0/1/2" "3?4#5#6"]))
  (is (= (split-first s #"#") ["0/1/2?3?4" "5#6"])))

(deftest split-first-2
  (is (= (split-first nil #"#") [nil nil]))
  (is (= (split-first "" #"#") [nil nil]))
  (is (= (split-first "a" #"#") ["a" nil]))
  (is (= (split-first "a#" #"#") ["a" nil]))
  (is (= (split-first "a#b" #"#") ["a" "b"]))
  (is (= (split-first "#b" #"#") [nil "b"]))
  (is (= (split-first "#" #"#") [nil nil])))

(deftest abc-1
  []
  (is (= (take 4 (abc)) '(:a :b :c :d)))
  (is (= (count (doall (abc))) 26))
  (is (= (last (abc)) :z))
  )

(deftest lpad-str-1
  []
  #?(:cljs
     (is (= "01" (contrib.string/lpad-str 2 "0" (str 1))))))

(deftest str-last-n-1
  (is (= (contrib.string/str-last-n 4 "0123456789")
         "6789"))
  )
