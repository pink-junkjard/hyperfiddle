(ns hypercrud.ui.attribute.tristate-boolean
  (:require [hypercrud.ui.control.link-controls :as link-controls]
            [hypercrud.ui.select :refer [select-boolean*]]))


(defn ^:export tristate-boolean [value ctx props]
  (let [path [(:fe-pos ctx) (:hypercrud.browser/attribute ctx)]]
    [:div
     [:div.editable-select {:key (:hypercrud.browser/attribute ctx)}
      [:div.anchors (link-controls/anchors path true ctx)]
      (select-boolean* value props ctx)]
     (link-controls/iframes path true ctx)]))
