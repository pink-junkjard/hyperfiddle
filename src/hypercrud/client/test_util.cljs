(ns hypercrud.client.test-util
  (:require-macros [cljs.test :refer [is]])
  (:require [clojure.set :refer [difference]]
            [hypercrud.client.tx :refer [into-tx]]))


(defn check-tx [in expected-out]
  (let [out (into-tx [] in)]
    (do (is (= (count expected-out) (count out)))
        (let [expected-out (set expected-out)
              out (set out)]
          (do (is (empty? (difference out expected-out)) "Unexpected datoms")
              (is (empty? (difference expected-out out)) "Missing datoms"))))))
