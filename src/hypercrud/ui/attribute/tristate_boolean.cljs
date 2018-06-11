(ns hypercrud.ui.attribute.tristate-boolean
  (:require
    [hypercrud.ui.select :refer [select-boolean*]]))


(defn ^:export tristate-boolean [value ctx props]
  [:div
   (select-boolean* value props ctx)])
