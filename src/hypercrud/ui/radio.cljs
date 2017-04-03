(ns hypercrud.ui.radio
  (:require [hypercrud.ui.tooltip :as tooltip]))


(defn option [props]
  [tooltip/hover-tooltip {:label (:tooltip props)}
   [:label.radio-option
    [:input {:type "radio"
             :style {:width "auto"}
             :checked (= (:value props) (:target props))
             :on-change #((:change! props) (:target props))
             :disabled (:disabled props)}]
    (:label props)]])
