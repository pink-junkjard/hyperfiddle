(ns hypercrud.browser.dbname
  (:require [clojure.string :as string]))


(defn uri->dbname [uri ctx]
  (get (->> (get-in ctx [:repository :repository/environment])
            (filter (fn [[k v]]
                      (and (string? k) (string/starts-with? k "$"))))
            (map (fn [[k v]]
                   [v k]))
            (into {}))
       uri))
