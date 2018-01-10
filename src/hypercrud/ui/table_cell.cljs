(ns hypercrud.ui.table-cell
  (:require [clojure.string :as string]
            [hypercrud.ui.control.link-controls :as link-controls]))


(defn ellipsis
  ([s] (ellipsis 25 s))
  ([c s] (if (> c (count s))
           s
           (str (subs s 0 (- c 3)) "..."))))

(defn ref-one-component [field props ctx]
  [:div
   #_(pr-str (:db/id @(:value ctx)))
   [:div.anchors (link-controls/render-links (remove :link/render-inline? (:links ctx)) ctx)]
   (link-controls/render-inline-links (filter :link/render-inline? (:links ctx)) ctx)])

(defn ref-many [field props ctx]
  [:div
   #_(->> (mapv :db/id @(:value ctx))
          (pr-str)
          (ellipsis 15))
   [:div.anchors (link-controls/render-links (remove :link/render-inline? (:links ctx)) ctx)]
   (link-controls/render-inline-links (filter :link/render-inline? (:links ctx)) ctx)])

(defn other-many [field props ctx]
  [:div
   [:button {:on-click #(js/alert "todo")} "Edit"]
   " "
   (->> (map pr-str @(:value ctx))                     ;todo account for many different types of values
        (string/join ", "))])
