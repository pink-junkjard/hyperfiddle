(ns hypercrud.browser.pages.index
  (:require [cemerick.url :as url]))


(defn view [index-queries]
  [:div
   [:ul.links
    (map (fn [[k v]]
           [:li {:key k} [:a {:href (str "query/" (url/url-encode v))} k]])
         index-queries)]])

(def commands {})
