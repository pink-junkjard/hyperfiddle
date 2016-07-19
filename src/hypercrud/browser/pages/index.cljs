(ns hypercrud.browser.pages.index
  (:require [hypercrud.browser.base-64-url-safe :as base64]))


(defn view [index-queries]
  [:div
   [:ul.links
    (map (fn [[k [metatype query]]]
           [:li {:key k} [:a {:href (str (name metatype) "/query/" (base64/encode query))} k]])
         index-queries)]])

(def commands {})
