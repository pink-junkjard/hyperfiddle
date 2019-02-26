(ns hyperfiddle.io.basis-test
  (:require
    [clojure.test :refer [deftest is]]
    [hyperfiddle.io.basis :as basis]))


(deftest compare-uri-maps-test []
  (is (= 0 (basis/compare-uri-maps {"$x" 1}
                                   {"$x" 1})))

  (is (= -1 (basis/compare-uri-maps {"$x" 0}
                                    {"$x" 1})))

  (is (= 1 (basis/compare-uri-maps {"$x" 1}
                                   {"$x" 0}))))

(deftest compare-different-ordering []
  (is (= 0 (basis/compare-uri-maps {"$a" 1 "$b" 2}
                                   {"$b" 2 "$a" 1}))))

(deftest compare-mismatched-uris []
  (is (thrown-with-msg? #?(:clj RuntimeException :cljs js/Error)
                        (re-pattern basis/ERROR-MISMATCHED-DBNAMES)
                        (basis/compare-uri-maps {"$x" 1}
                                                {"$y" 1})))

  (is (thrown-with-msg? #?(:clj RuntimeException :cljs js/Error)
                        (re-pattern basis/ERROR-MISMATCHED-DBNAMES)
                        (basis/compare-uri-maps {"$x" 1}
                                                {"$x" 1
                                                 "$y" 1}))))

(deftest compare-greater-and-less-than []
  (is (thrown-with-msg? #?(:clj RuntimeException :cljs js/Error)
                        (re-pattern basis/ERROR-BOTH-GREATER-AND-LESS-THAN)
                        (basis/compare-uri-maps {"$x" 2
                                                 "$y" 1}
                                                {"$x" 1
                                                 "$y" 2}))))
