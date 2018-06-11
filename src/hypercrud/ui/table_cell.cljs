(ns hypercrud.ui.table-cell
  (:require
    [clojure.string :as string]))


(defn ellipsis
  ([s] (ellipsis 25 s))
  ([c s] (if (> c (count s))
           s
           (str (subs s 0 (- c 3)) "..."))))

(defn ref-one-component [value ctx props]
  [:div])

(defn ref-many [value ctx props]
  [:div #_(->> (mapv :db/id @(:value ctx))
               (pr-str)
               (ellipsis 15))])

(defn other-many [value ctx props]
  [:div
   [:button {:on-click #(js/alert "todo")} "Edit"]
   " "
   (->> (map pr-str @(:value ctx))                          ;todo account for many different types of values
        (string/join ", "))])
