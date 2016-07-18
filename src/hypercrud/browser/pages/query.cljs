(ns hypercrud.browser.pages.query
  (:require [hypercrud.client.core :as hc]
            [hypercrud.ui.collection :refer [cj-grid]]))


(defn view [query client forms]
  [cj-grid client forms (hc/select client ::query)])


(def commands {})
