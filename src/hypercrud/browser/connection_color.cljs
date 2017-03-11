(ns hypercrud.browser.connection-color
  (:require [hypercrud.client.core :as hc]))


; http://martin.ankerl.com/2009/12/09/how-to-create-random-colors-programmatically/

(def golden-ratio 0.618033988749895)
(def seed 0.3100632204946232 #_(.random js/Math))           ; i liked these colors

(def connection-color
  (memoize
    (fn [conn-id]
      (let [n (if conn-id (- conn-id hc/*root-conn-id*))]
        (case n
          nil "#fff"
          0 "#777"                                          ; root
          (js/Color (clj->js {:h (* 360 (mod (+ seed (* n golden-ratio)) 1))
                              :s 55 #_"Too bright hurts the eyes"
                              :l 50 #_"Medium gray can be read on white and black backgrounds"
                              })))))))
