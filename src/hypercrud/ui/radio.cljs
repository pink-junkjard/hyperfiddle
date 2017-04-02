(ns hypercrud.ui.radio
  (:require [hypercrud.ui.tooltip :as tooltip]))


(defn option [label target value change! & [tooltip]]
  [tooltip/hover-tooltip {:label tooltip}
   [:label
    [:input {:type "radio" :style {:width "auto"}
             :checked (= value target) :on-change #(change! target)}]
    label]])
