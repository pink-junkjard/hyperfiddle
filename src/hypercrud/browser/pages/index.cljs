(ns hypercrud.browser.pages.index
  (:require [hypercrud.browser.base-64-url-safe :as base64]))


(defn ui [queries navigate-cmp]
  [:div
   ;form to populate holes
   [:ul.links
    (map (fn [{:keys [:query/ident :query/value :query/form :query/hole]}]
           (let [[q _] value
                 hp (->> hole
                         (map (juxt :hole/name (constantly nil)))
                         (into {}))]
             [:li {:key ident}
              [navigate-cmp {:href (str form "/query/" (base64/encode {:q q :hp hp}))} ident]]))
         queries)]])

(defn query [] {})
