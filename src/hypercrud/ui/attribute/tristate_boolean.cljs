(ns hypercrud.ui.attribute.tristate-boolean
  (:require [hypercrud.ui.control.link-controls :as link-controls]
            [hypercrud.ui.select :refer [select-boolean*]]))


(defn tristate-boolean [maybe-field props ctx]
  (let [path [(:fe-pos ctx) (-> ctx :attribute :db/ident)]]
    [:div.value
     [:div.editable-select {:key (:db/ident (:attribute ctx))}
      [:div.anchors (link-controls/render-nav-cmps path true ctx)]
      (select-boolean* @(:value ctx) props ctx)]
     (link-controls/render-inline-links path true ctx)]))
