(ns hypercrud.browser.pages.index
  (:require [hypercrud.browser.base-64-url-safe :as base64]))


(defn ui [queries navigate-cmp]
  [:div
   ;form to populate holes
   [:ul.links
    (->> (vals queries)
         (filter #(not= nil (:query/form %)))
         (sort-by :query/ident)
         (map (fn [{:keys [:query/ident :query/value :query/form]}]
                [:li {:key ident}
                 [navigate-cmp {:href (str form "/query/" (base64/encode value))} ident]])))]])

(defn query [] {})
