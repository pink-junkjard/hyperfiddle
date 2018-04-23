(ns hypercrud.ui.control.checkbox)


(defn checkbox* [r change! & [props]]
  [:input {:type "checkbox"
           :checked @r
           :disabled (:read-only props)
           :on-change change!}])
