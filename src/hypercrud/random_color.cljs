(ns hypercrud.random-color)



; http://martin.ankerl.com/2009/12/09/how-to-create-random-colors-programmatically/

(def golden-ratio 0.618033988749895)
(def seed .259                                              ; (js/Math.random)
  )
(def hue (atom seed))


(defn random-color []
  (swap! hue (fn [v] (mod (+ v golden-ratio) 1)) golden-ratio)
  (js/Color (clj->js {:h (* 360 @hue)
                      :s 80
                      :v 80})))
