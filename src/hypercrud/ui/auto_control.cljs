(ns hypercrud.ui.auto-control
  (:require [hypercrud.client.core :as hc]
            [hypercrud.random-color :refer [random-color]]))


(defmulti auto-control (fn [] :default))

(defmulti auto-table-cell (fn [] :default))

(defmulti resultset (fn [] :default))


(def connection-color
  (memoize
    (fn [conn-id]
      (if (= conn-id hc/*root-conn-id*) "#777" (random-color)))))
