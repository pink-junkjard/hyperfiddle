(ns hypercrud.ui.table-cell
  (:require [clojure.string :as string]
            [hypercrud.ui.widget :as widget]))


(defn ellipsis
  ([s] (ellipsis 25 s))
  ([c s] (if (> c (count s))
           s
           (str (subs s 0 (- c 3)) "..."))))


(defn ref-one-component [value field anchors props param-ctx]
  [:div
   #_(pr-str (:db/id value))
   [:div.anchors (widget/render-anchors (remove :anchor/render-inline? anchors) param-ctx)]
   (widget/render-inline-links field (filter :anchor/render-inline? anchors) param-ctx)])


(defn ref-many [value field anchors props param-ctx]
  [:div
   #_(->> (mapv :db/id value)
          (pr-str)
          (ellipsis 15))
   [:div.anchors (widget/render-anchors (remove :anchor/render-inline? anchors) param-ctx)]
   (widget/render-inline-links field (filter :anchor/render-inline? anchors) param-ctx)])


(defn other-many [value field anchors props param-ctx]
  [:div
   [:button {:on-click #(js/alert "todo")} "Edit"]
   " "
   (->> (map pr-str value)                                  ;todo account for many different types of values
        (string/join ", "))])
