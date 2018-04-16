(ns contrib.eval-test
  (:require [cats.monad.either :refer [#?(:cljs Left)]]
            [clojure.test :refer [deftest is]]
            [contrib.eval :refer [eval-str-and-throw eval-str]]
            [contrib.macros :refer [str-and-code]]
            [taoensso.timbre :as timbre])
  #?(:clj
     (:import [cats.monad.either Left])))


(deftest test-eval []
  (is (= 1 (eval-str-and-throw "1")))
  (is (= 2 ((eval-str-and-throw "(constantly 2)"))))
  (is (= 3 ((eval-str-and-throw (str-and-code (constantly 3))))))

  (is (nil? (eval-str-and-throw "")))
  (is (nil? (eval-str-and-throw " ")))
  (is (nil? (eval-str-and-throw nil)))
  (is (nil? (eval-str-and-throw (constantly 1))))

  (timbre/with-config {:enabled? false}
    (is (instance? Left (eval-str ")")))))
