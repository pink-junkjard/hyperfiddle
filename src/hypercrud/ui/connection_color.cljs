(ns hypercrud.ui.connection-color)


; http://martin.ankerl.com/2009/12/09/how-to-create-random-colors-programmatically/

(def golden-ratio 0.618033988749895)
(def seed 0.3100632204946232 #_(.random js/Math))           ; i liked these colors

(def no-conn "#ccc")
(def root-conn "#777")

(defn connection-color [ctx & [l]]
  (let [uri (:uri ctx)]
    (condp = uri
      nil no-conn
      (get-in ctx [:hypercrud.browser/domain :domain/fiddle-repo]) root-conn
      (-> {:h (* 360 (mod (+ seed (* (hash uri) golden-ratio)) 1))
           :s 55 #_"Too bright hurts the eyes"
           :l (or l 70) #_"Medium gray (50) can be read on white and black backgrounds"}
          (clj->js)
          (js/Color)
          (.string)))))
