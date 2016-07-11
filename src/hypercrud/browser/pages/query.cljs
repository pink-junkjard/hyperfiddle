(ns hypercrud.browser.pages.query
  (:require [hypercrud.browser.base-64-url-safe :as base64]
            [hypercrud.client.reagent :as hcr]
            [hypercrud.ui.collection :refer [cj-grid]]))


(defn view [query client forms]
  [hcr/query client (base64/decode query)
   (fn [eids]
     [cj-grid client forms eids])])


(comment
  ;; sorting test
  [hcr/query client (base64/decode query)
   (fn [eids]
     [hcr/promised client
      (hc-sort/hc-sort-by client eids [#(p/resolved (:community/url %))])
      (fn [eids]
        [cj-grid client forms eids])])])


(def commands {})
