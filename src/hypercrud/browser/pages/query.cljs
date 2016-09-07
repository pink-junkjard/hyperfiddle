(ns hypercrud.browser.pages.query
  (:require [hypercrud.ui.table :as table]
            [hypercrud.client.core :as hc]))


(defn ui [graph forms form-id]
  [:div
   [table/table graph forms (hc/select graph ::table/query) form-id]
   [:a {:href (str "../entity/-1")} "Create"]])


(defn query [form query]
  (table/query form query))
