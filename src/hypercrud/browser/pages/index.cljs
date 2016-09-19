(ns hypercrud.browser.pages.index
  (:require [hypercrud.browser.base-64-url-safe :as base64]))


(defn ui [queries navigate-cmp]
  [:div
   ;form to populate holes
   [:ul.links
    (map (fn [{:keys [:query/ident :query/value :query/form]}]
           (let [[q args] value]
             [:li {:key ident}
              [navigate-cmp {:href (str form "/query/" (base64/encode q))} ident]]))
         queries)]])

(defn query [] {})
