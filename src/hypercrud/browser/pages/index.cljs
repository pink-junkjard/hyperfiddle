(ns hypercrud.browser.pages.index
  (:require [hypercrud.browser.base-64-url-safe :as base64]))


(defn ui [query->form queries]
  [:div
   ;form to populate holes
   [:ul.links
    (map (fn [[query-name form-name]]
           (let [[q args] (get queries query-name)]
             [:li {:key query-name} [:a {:href (str (name form-name) "/query/" (base64/encode q))} query-name]]))
         query->form)]])

(defn query [] {})
