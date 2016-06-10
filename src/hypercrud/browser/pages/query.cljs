(ns hypercrud.browser.pages.query
  (:require [cemerick.url :as url]
            [hypercrud.client.core :as hypercrud]
            [hypercrud.ui.collection :refer [cj-grid]]))


(defn view [query client forms]
  [hypercrud/resolve-query client (url/url-decode query)
   (fn [eids]
     [cj-grid client forms eids])])


(def commands {})
