(ns hypercrud.browser.pages.index
  (:require [hypercrud.browser.base-64-url-safe :as base64]))


(defn ui [queries navigate-cmp]
  [:div
   ;form to populate holes
   [:ul.links
    (->> (vals queries)
         (filter #(not= nil (:query/form %)))
         (map (fn [{:keys [:query/ident :query/value :query/form :query/hole]}]
                (let [hp {} #_(->> hole
                                   (map (juxt :hole/name (constantly nil)))
                                   (into {}))]
                  [:li {:key ident}
                   [navigate-cmp {:href (str form "/query/" (base64/encode {:q value :hp hp}))} ident]]))))]])

(defn query [] {})
