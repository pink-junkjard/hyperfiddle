(ns contrib.css
  (:require [cuerdas.core :as str]))


(defn css-slugify [s]
  ; http://stackoverflow.com/a/449000/959627
  ; https://mathiasbynens.be/notes/css-escapes
  (-> (str s)                                               ; coerce keywords etc
      (str/replace ":" "-")
      (str/replace "/" "-")
      (str/replace "?" "-")                                 ; legal but syntax highlighting issues
      (str/replace " " "-")
      (str/replace "." "-")
      (str/replace ")" "-")
      (str/replace "(" "-")
      ))

(defn classes
  "&args will be flattened"
  [& args]
  (->> args
       flatten
       (remove nil?)
       #_(map css-slugify)                                  ; cannot do this because sometimes we pass pre-concat css strings here
       (str/join " ")))
