(ns hypercrud.ui.control.checkbox
  (:require))


(defn checkbox* [r change! & [props]]
  [:input {:type "checkbox"
           :checked @r
           :on-change change!}])
