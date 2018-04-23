(ns hyperfiddle.io.global-basis-test
  (:refer-clojure :exclude [compare])
  (:require [clojure.test :refer [deftest is]]
            [hyperfiddle.io.global-basis :as global-basis :refer [compare]]))


(deftest compare-identical []
  (is (= 0 (compare {:domain {#uri "domains" 0}
                     :ide {#uri "A" 1}
                     :user {#uri "X" 1}}
                    {:domain {#uri "domains" 0}
                     :ide {#uri "A" 1}
                     :user {#uri "X" 1}}))))

(deftest compare-different-domain-t []
  (is (= -1 (compare {:domain {#uri "domains" 0}
                      :ide {#uri "A" 1}
                      :user {#uri "X" 1}}
                     {:domain {#uri "domains" 1}
                      :ide {#uri "B" 1}
                      :user {#uri "Y" 1}})))

  (is (= 1 (compare {:domain {#uri "domains" 1}
                     :ide {#uri "B" 1}
                     :user {#uri "Y" 1}}
                    {:domain {#uri "domains" 0}
                     :ide {#uri "A" 1}
                     :user {#uri "Y" 1}}))))

(deftest compare-user-ide-t []
  (is (= -1 (compare {:domain {#uri "domains" 0}
                      :ide {#uri "A" 0}
                      :user {#uri "X" 1}}
                     {:domain {#uri "domains" 0}
                      :ide {#uri "A" 1}
                      :user {#uri "X" 1}})))

  (is (= 0 (compare {:domain {#uri "domains" 0}
                     :ide {#uri "A" 1}
                     :user {#uri "X" 1}}
                    {:domain {#uri "domains" 0}
                     :ide {#uri "A" 1}
                     :user {#uri "X" 1}})))

  (is (= 1 (compare {:domain {#uri "domains" 0}
                     :ide {#uri "A" 1}
                     :user {#uri "X" 1}}
                    {:domain {#uri "domains" 0}
                     :ide {#uri "A" 1}
                     :user {#uri "X" 0}}))))

(deftest compare-mismatched-uris []
  (is (thrown-with-msg? #?(:clj RuntimeException :cljs js/Error)
                        (re-pattern global-basis/ERROR-MISMATCHED-URIS)
                        (compare {:domain {#uri "NOT-domains" 0}
                                  :ide {#uri "A" 1}
                                  :user {#uri "X" 1}}
                                 {:domain {#uri "domains" 0}
                                  :ide {#uri "A" 1}
                                  :user {#uri "X" 1}})))

  (is (thrown-with-msg? #?(:clj RuntimeException :cljs js/Error)
                        (re-pattern global-basis/ERROR-MISMATCHED-URIS)
                        (compare {:domain {#uri "domains" 0}
                                  :ide {#uri "A" 1}
                                  :user {#uri "X" 1}}
                                 {:domain {#uri "domains" 0}
                                  :ide {#uri "B" 1}
                                  :user {#uri "X" 1}})))

  (is (thrown-with-msg? #?(:clj RuntimeException :cljs js/Error)
                        (re-pattern global-basis/ERROR-MISMATCHED-URIS)
                        (compare {:domain {#uri "domains" 0}
                                  :ide {#uri "A" 1}
                                  :user {#uri "X" 1}}
                                 {:domain {#uri "domains" 0}
                                  :ide {#uri "A" 1
                                        #uri "I" 1}
                                  :user {#uri "X" 1}}))))

(deftest compare-greater-and-less-than []
  (is (thrown-with-msg? #?(:clj RuntimeException :cljs js/Error)
                        (re-pattern global-basis/ERROR-BOTH-GREATER-AND-LESS-THAN)
                        (compare {:domain {#uri "domains" 0}
                                  :ide {#uri "A" 1}
                                  :user {#uri "X" 2}}
                                 {:domain {#uri "domains" 0}
                                  :ide {#uri "A" 2}
                                  :user {#uri "X" 1}}))))
