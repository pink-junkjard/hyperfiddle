(ns hypercrud.browser.core
  (:require [cljs.core.match :refer-macros [match]]
            [cljs.pprint :as pprint]
            [clojure.string :as string]
            [hypercrud.browser.base-64-url-safe :as base64]
            [hypercrud.browser.pages.entity :as entity]
            [hypercrud.browser.pages.index :as index]
            [hypercrud.client.core :as hc]
            [hypercrud.ui.collection :as collection :refer [cj-grid cj-grid-graph-deps]]
            [hypercrud.ui.form :refer [cj-form-dependencies]]))


(defn graph-dependencies [forms state page-rel-path]
  (match [(string/split page-rel-path "/")]
         [[metatype "query" q]] (cj-grid-graph-deps ((keyword metatype) forms) (base64/decode q))
         [[metatype "entity" eid]] (cj-form-dependencies eid (get state :expanded nil) ((keyword metatype) forms) forms)
         [[""]] {}
         :else {}))


(defn ui [cur transact! graph forms index-queries page-rel-path]
  [:div
   [:div.hc-node-view
    (match [(string/split page-rel-path "/")]
           [[metatype "query" q]] [cj-grid graph forms (hc/select graph ::collection/query) (keyword metatype)]
           [[metatype "entity" eid]] (entity/view cur transact! graph (keyword metatype) forms (js/parseInt eid 10))
           [[""]] (index/view index-queries)
           :else [:div "no route for: " page-rel-path])]
   [:hr]
   [:pre (with-out-str (pprint/pprint @cur))]])
