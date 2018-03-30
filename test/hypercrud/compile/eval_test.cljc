(ns hypercrud.compile.eval-test
  (:require [cats.monad.either :refer [#?(:cljs Left)]]
            [#?(:clj clojure.test :cljs cljs.test) #?(:clj :refer :cljs :refer-macros) [deftest is]]
            [hypercrud.compile.eval :as eval]
            [contrib.macros :refer [str-and-code]]
            [taoensso.timbre :as timbre])
  #?(:clj
     (:import [cats.monad.either Left])))


(deftest test-eval []
  (is (= 1 (eval/eval-str-and-throw "1")))
  (is (= 2 ((eval/eval-str-and-throw "(constantly 2)"))))
  (is (= 3 ((eval/eval-str-and-throw (str-and-code (constantly 3))))))

  (is (nil? (eval/eval-str-and-throw "")))
  (is (nil? (eval/eval-str-and-throw " ")))
  (is (nil? (eval/eval-str-and-throw nil)))
  (is (nil? (eval/eval-str-and-throw (constantly 1))))

  (timbre/with-config {:enabled? false}
    (is (instance? Left (eval/eval-str ")")))))
