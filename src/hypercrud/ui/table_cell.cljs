(ns hypercrud.ui.table-cell
  (:require [clojure.string :as string]
            [hypercrud.ui.widget :as widget]))


(defn ellipsis
  ([s] (ellipsis 25 s))
  ([c s] (if (> c (count s))
           s
           (str (subs s 0 (- c 3)) "..."))))

(defn ref-one-component [field anchors props ctx]
  [:div
   #_(pr-str (:db/id (:value ctx)))
   [:div.anchors (widget/render-links (remove :link/render-inline? anchors) ctx)]
   (widget/render-inline-links (filter :link/render-inline? anchors) ctx)])

(defn ref-many [field anchors props ctx]
  [:div
   #_(->> (mapv :db/id (:value ctx))
          (pr-str)
          (ellipsis 15))
   [:div.anchors (widget/render-links (remove :link/render-inline? anchors) ctx)]
   (widget/render-inline-links (filter :link/render-inline? anchors) ctx)])

(defn other-many [field anchors props ctx]
  [:div
   [:button {:on-click #(js/alert "todo")} "Edit"]
   " "
   (->> (map pr-str (:value ctx))                     ;todo account for many different types of values
        (string/join ", "))])
