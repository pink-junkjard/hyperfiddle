(ns hypercrud.browser.pages.index
  (:require [hypercrud.browser.links :as links]))


(defn ui [queries navigate-cmp]
  [:div
   ;form to populate holes
   [:ul.links
    (->> (vals queries)
         (filter #(not= nil (:query/form %)))
         (sort-by :query/ident)
         (map (fn [{:keys [:query/ident :query/value :query/form] :as query}]
                [:li {:key ident}
                 [navigate-cmp {:href (links/query-link form value nil)} ident]])))]])

(defn query [] {})
