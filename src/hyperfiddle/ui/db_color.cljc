(ns hyperfiddle.ui.db-color
  (:require
    [cuerdas.core :as str]))


; http://martin.ankerl.com/2009/12/09/how-to-create-random-colors-programmatically/

(def golden-ratio 0.618033988749895)
(def seed 0.3100632204946232 #_(.random js/Math))           ; i liked these colors

(defn hsl [h s l] (str/format "hsl(%s, %s%, %s%)" h s l))

(defn color-for-name [dbname]
  (condp = dbname
    nil "#ccc"
    (hsl (* 360 (mod (+ seed (* (hash dbname) golden-ratio)) 1))
         55  #_"Too bright hurts the eyes"
         70) #_"Medium gray (50) can be read on white and black backgrounds"
    ))
