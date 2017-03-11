(ns hypercrud.browser.connection-color
  (:require [hypercrud.client.core :as hc]
            [hypercrud.random-color :as random-color]))


(def connection-color
  (memoize
    (fn [conn-id]
      (case conn-id
        nil "#fff"
        hc/*root-conn-id* "#777"
        (random-color/random-color)))))
