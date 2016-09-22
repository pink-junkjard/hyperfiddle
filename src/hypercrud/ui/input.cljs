(ns hypercrud.ui.input
  (:require [reagent.core :as reagent]))


; TODO bug - if value controlled from above, this will ignore it
(defn validated-input [value on-change! parse-string to-string valid? & [props]]
  (let [intermediate-val (reagent/atom (to-string value))]
    (fn [value on-change! parse-string to-string valid?]
      [:input (merge props {:type "text"
                            :class (if-not (valid? @intermediate-val) "invalid")
                            :value @intermediate-val
                            :on-change #(reset! intermediate-val (.. % -target -value))
                            :on-blur #(if (valid? @intermediate-val)
                                       (on-change! (parse-string @intermediate-val)))})])))


(defn input* [value on-change! & [props]]
  [validated-input value on-change! identity identity (constantly true) props])
