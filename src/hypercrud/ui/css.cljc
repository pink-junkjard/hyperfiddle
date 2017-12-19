(ns hypercrud.ui.css
  (:require [cuerdas.core :as str]))


(defn classes
  "&args will be flattened"
  [& args]
  (->> args flatten (remove nil?) (str/join " ")))
