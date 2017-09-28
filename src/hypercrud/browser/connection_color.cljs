(ns hypercrud.browser.connection-color
  (:require [hypercrud.client.core :as hc]))


; http://martin.ankerl.com/2009/12/09/how-to-create-random-colors-programmatically/

(def golden-ratio 0.618033988749895)
(def seed 0.3100632204946232 #_(.random js/Math))           ; i liked these colors

(defn connection-color [uri & [l]]
  (condp = uri
    nil "#000"
    hc/*root-conn-id* "#777"
    (-> {:h (* 360 (mod (+ seed (* (hash uri) golden-ratio)) 1))
         :s 55 #_"Too bright hurts the eyes"
         :l (or l 50) #_"Medium gray (50) can be read on white and black backgrounds"}
        (clj->js)
        (js/Color)
        (.string))))
