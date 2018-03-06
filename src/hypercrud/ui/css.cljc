(ns hypercrud.ui.css
  (:require [cuerdas.core :as str]))


(defn css-slugify [s]
  ; http://stackoverflow.com/a/449000/959627
  (-> (str s)                                               ; coerce keywords etc
      (str/replace ":" "-")
      (str/replace "/" "-")
      (str/replace "?" "-")                                 ; legal but syntax highlighting issues
      (str/replace " " "-")))

(defn classes
  "&args will be flattened"
  [& args]
  (->> args flatten (remove nil?) (map css-slugify) (str/join " ")))
