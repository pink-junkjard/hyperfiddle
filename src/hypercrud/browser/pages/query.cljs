(ns hypercrud.browser.pages.query
  (:require [cemerick.url :as url]
            [hypercrud.client.reagent :as hcr]
            [hypercrud.ui.collection :refer [cj-grid]]))


(defn view [query client forms]
  [hcr/query client (url/url-decode query)
   (fn [eids]
     [cj-grid client forms eids])])


(def commands {})
