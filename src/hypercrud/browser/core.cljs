(ns hypercrud.browser.core
  (:require [cljs.core.match :refer-macros [match]]
            [cljs.pprint :as pprint]
            [clojure.string :as string]
            [hypercrud.browser.base-64-url-safe :as base64]
            [hypercrud.browser.pages.entity :as entity]
            [hypercrud.browser.pages.index :as index]
            [hypercrud.client.core :as hc]
            [hypercrud.ui.table :as table]))


(defn query [forms state page-rel-path]
  (match [(string/split page-rel-path "/")]
         [[metatype "query" q]] (table/query ((keyword metatype) forms) (base64/decode q))
         [[metatype "entity" eid]] (entity/query (js/parseInt eid 10) state (keyword metatype) forms)
         [[""]] {}
         :else {}))


(defn ui [cur transact! graph forms index-queries page-rel-path]
  [:div
   [:div.hc-node-view
    (match [(string/split page-rel-path "/")]
           [[metatype "query" q]] [table/table graph forms (hc/select graph ::table/query) (keyword metatype)]
           [[metatype "entity" eid]] (entity/ui cur transact! graph (keyword metatype) forms (js/parseInt eid 10))
           [[""]] (index/view index-queries)
           :else [:div "no route for: " page-rel-path])]
   [:hr]
   [:pre (with-out-str (pprint/pprint @cur))]])
