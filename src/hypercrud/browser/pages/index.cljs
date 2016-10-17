(ns hypercrud.browser.pages.index
  (:require [hypercrud.browser.links :as links]))


(defn ui [links queries navigate-cmp]
  [:div
   ;form to populate holes
   [:ul.links
    (->> links
         (sort-by :link/prompt)
         (map (fn [{:keys [:link/prompt :link/ident :link/query :link/form] :as link}]
                [:li {:key ident}
                 [navigate-cmp {:href (links/query-link form (:query/value (get queries query)) nil)} prompt]])))]])

(defn query [] {})
