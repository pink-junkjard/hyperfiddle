(ns hypercrud.ui.css
  (:require [cuerdas.core :as str]))


(defn classes
  "&args will be flattened"
  [& args]
  (->> args flatten (remove nil?) (str/join " ")))

(defn css-slugify [s]
  ; http://stackoverflow.com/a/449000/959627
  (-> (str s)                                               ; coerce keywords etc
      (str/replace ":" "-")
      (str/replace "/" "-")
      (str/replace " " "-")))
