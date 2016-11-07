(ns hypercrud.browser.pages.index
  (:require [hypercrud.browser.links :as links]
            [hypercrud.client.core :as hc]))


(defn ui [links navigate-cmp param-ctx]
  [:div
   ;form to populate holes
   [:ul.links
    (->> links
         (sort-by :link/prompt)
         (map (fn [{:keys [:link/prompt :link/ident] :as link}]
                [:li {:key ident}
                 [navigate-cmp {:href (links/query-link link param-ctx)} prompt]])))]])


(defn query [] {})
