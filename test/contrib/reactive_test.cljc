(ns contrib.reactive-test
  (:require
    [cats.monad.either :as either]
    [clojure.test :refer [deftest is]]
    [contrib.reactive :as r]))


(deftest test-constantly []
  #?(:cljs                                                  ; clj implementation not yet implemented
     (is (= (r/constantly 1) (r/constantly 1))))
  (is (not= (r/constantly 1) nil))
  (is (= 1 ((r/constantly 1) "asdf"))))

(deftest test-comp []
  (is (not= (comp inc inc) (comp inc inc)))                 ; control
  #?(:cljs                                                  ; clj implementation not yet implemented
     (is (= (r/comp inc inc) (r/comp inc inc))))
  (is (not= (r/comp inc inc) nil))
  (is (= 3 ((r/comp inc inc) 1))))

(deftest test-fmap []
  (let [a (r/atom 1)]
    #?(:cljs                                                ; clj implementation not yet implemented
       (is (= (r/fmap inc a) (r/fmap inc a))))
    (is (= 2 @(r/fmap inc a)))))

(deftest test-fmap-> []
  (let [a (r/atom 1)]
    #?(:cljs                                                ; clj implementation not yet implemented
       (is (= (r/fmap-> a inc (* 10)) (r/fmap-> a inc (* 10)))))
    (is (= 20 @(r/fmap-> a inc (* 10))))))

(deftest test-fmap->> []
  (let [a (r/atom 1)]
    #?(:cljs                                                ; clj implementation not yet implemented
       (is (= (r/fmap->> a inc (* 10)) (r/fmap->> a inc (* 10)))))
    (is (= 20 @(r/fmap->> a inc (* 10))))))

(deftest test-fapply []
  (let [reactive-inc (r/atom inc)]
    (is (= 2 @(r/fapply reactive-inc (r/atom 1)))))

  (let [reactive-inc (r/atom (fn [a] (fn [b] (+ a b))))]
    (is (= 3 @(r/fapply reactive-inc (r/atom 1) (r/atom 2))))))

(deftest test-apply-inner-r []
  (let [state (r/atom (either/right 1))
        before @@(r/apply-inner-r state)]
    (is (= 1 @before))
    (reset! state (either/right 2))
    (let [after @@(r/apply-inner-r state)]
      #?(:cljs                                              ; clj implementation not yet implemented
         (is (= before after)))
      #?(:cljs                                              ; clj implementation not yet implemented
         (is (= 2 @before @after))))))
