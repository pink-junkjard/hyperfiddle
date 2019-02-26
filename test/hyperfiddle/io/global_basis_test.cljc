(ns hyperfiddle.io.global-basis-test
  (:require
    [clojure.test :refer [deftest is]]
    [hyperfiddle.io.global-basis :as global-basis]))


(deftest compare-identical []
  (is (= 0 (global-basis/compare {:domain 0
                                  :user {"$x" 1}}
                                 {:domain 0
                                  :user {"$x" 1}}))))

(deftest compare-different-domain-t []
  (is (= -1 (global-basis/compare {:domain 0
                                   :user {"$x" 1}}
                                  {:domain 1
                                   :user {"$y" 1}})))

  (is (= 1 (global-basis/compare {:domain 1
                                  :user {"$y" 1}}
                                 {:domain 0
                                  :user {"$y" 1}}))))
