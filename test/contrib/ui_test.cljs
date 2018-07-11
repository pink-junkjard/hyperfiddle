(ns contrib.ui-test
  (:require
    [clojure.core.match :refer [match match*]]
    [clojure.test :refer [deftest is]]
    [contrib.ui :refer [easy-checkbox]]
    [contrib.reactive :as r]))


(deftest checkbox-readonly
  []
  (let [control (easy-checkbox "" true #() {:read-only true})]
    (is
      (match [control]
        [[:input {:disabled true}]] true))))
 