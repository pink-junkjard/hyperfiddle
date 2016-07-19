(ns hypercrud.browser.pages.query
  (:require [hypercrud.client.core :as hc]
            [hypercrud.ui.collection :refer [cj-grid]]))


(defn view [query graph forms]
  [cj-grid graph forms (hc/select graph ::query)])


(def commands {})
