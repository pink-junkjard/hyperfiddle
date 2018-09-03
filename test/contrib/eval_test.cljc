(ns contrib.eval-test
  (:require [cats.monad.either :refer [#?(:cljs Left)]]
            [clojure.test :refer [deftest is]]
            [contrib.eval :refer [eval-string! safe-eval-string]]
            [taoensso.timbre :as timbre])
  #?(:clj
     (:import [cats.monad.either Left])))


(deftest test-eval-string []
  (is (= 1 (eval-string! "1")))
  (is (= 2 ((eval-string! "(constantly 2)"))))

  (timbre/with-config {:enabled? false}
    (is (instance? Left (safe-eval-string "")))
    (is (instance? Left (safe-eval-string " ")))
    (is (instance? Left (safe-eval-string nil)))
    (is (instance? Left (safe-eval-string ")")))))
