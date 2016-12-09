(ns hypercrud.form.find-elements
  (:require [hypercrud.util :as util]))


(defn order-find-elements [find-elements q]
  (let [find-element-lookup (->> (mapv (juxt :find-element/name identity) find-elements)
                                 (into {}))]
    (->> (util/parse-query-element q :find)
         (mapv str)
         (mapv #(get find-element-lookup %)))))
