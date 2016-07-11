(ns hypercrud.browser.pages.index
  (:require [hypercrud.browser.base-64-url-safe :as base64]))


(defn view [index-queries]
  [:div
   [:ul.links
    (map (fn [[k v]]
           [:li {:key k} [:a {:href (str "query/" (base64/encode v))} k]])
         index-queries)]])

(def commands {})
