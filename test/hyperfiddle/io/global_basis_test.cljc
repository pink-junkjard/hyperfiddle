(ns hyperfiddle.io.global-basis-test
  (:refer-clojure :exclude [compare])
  (:require
    [clojure.test :refer [deftest is]]
    [hyperfiddle.io.global-basis :as global-basis :refer [compare]]))


(deftest compare-identical []
  (is (= 0 (compare {:domain 0
                     :user {"$x" 1}}
                    {:domain 0
                     :user {"$x" 1}}))))

(deftest compare-different-domain-t []
  (is (= -1 (compare {:domain 0
                      :user {"$x" 1}}
                     {:domain 1
                      :user {"$y" 1}})))

  (is (= 1 (compare {:domain 1
                     :user {"$y" 1}}
                    {:domain 0
                     :user {"$y" 1}}))))

(deftest compare-user-t []
  (is (= -1 (compare {:domain 0
                      :user {"$x" 0}}
                     {:domain 0
                      :user {"$x" 1}})))

  (is (= 1 (compare {:domain 0
                     :user {"$x" 1}}
                    {:domain 0
                     :user {"$x" 0}}))))

(deftest compare-mismatched-uris []
  (is (thrown-with-msg? #?(:clj RuntimeException :cljs js/Error)
                        (re-pattern global-basis/ERROR-MISMATCHED-DBNAMES)
                        (compare {:domain 0
                                  :user {"$x" 1}}
                                 {:domain 0
                                  :user {"$y" 1}})))

  (is (thrown-with-msg? #?(:clj RuntimeException :cljs js/Error)
                        (re-pattern global-basis/ERROR-MISMATCHED-DBNAMES)
                        (compare {:domain 0
                                  :user {"$x" 1}}
                                 {:domain 0
                                  :user {"$x" 1
                                         "$y" 1}}))))

(deftest compare-greater-and-less-than []
  (is (thrown-with-msg? #?(:clj RuntimeException :cljs js/Error)
                        (re-pattern global-basis/ERROR-BOTH-GREATER-AND-LESS-THAN)
                        (compare {:domain 0
                                  :user {"$x" 2
                                         "$y" 1}}
                                 {:domain 0
                                  :user {"$x" 1
                                         "$y" 2}}))))
