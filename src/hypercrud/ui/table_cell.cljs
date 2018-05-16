(ns hypercrud.ui.table-cell
  (:require [clojure.string :as string]
            [hypercrud.ui.control.link-controls :as link-controls]))


(defn ellipsis
  ([s] (ellipsis 25 s))
  ([c s] (if (> c (count s))
           s
           (str (subs s 0 (- c 3)) "..."))))

(defn ref-one-component [props ctx]
  (let [path [(:fe-pos ctx) (:hypercrud.browser/attribute ctx)]]
    [:div
     #_(pr-str (:db/id @(:value ctx)))
     [:div.anchors (link-controls/anchors path true ctx)]
     (link-controls/iframes path true ctx)]))

(defn ref-many [props ctx]
  (let [path [(:fe-pos ctx) (:hypercrud.browser/attribute ctx)]]
    [:div
     #_(->> (mapv :db/id @(:value ctx))
            (pr-str)
            (ellipsis 15))
     [:div.anchors (link-controls/anchors path true ctx)]
     (link-controls/iframes path true ctx)]))

(defn other-many [props ctx]
  [:div
   [:button {:on-click #(js/alert "todo")} "Edit"]
   " "
   (->> (map pr-str @(:value ctx))                          ;todo account for many different types of values
        (string/join ", "))])
