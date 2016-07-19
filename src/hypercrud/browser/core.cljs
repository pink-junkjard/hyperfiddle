(ns hypercrud.browser.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [<! chan]]
            [cljs.core.match :refer-macros [match]]
            [cljs.pprint :as pprint]
            [clojure.string :as string]
            [hypercrud.browser.base-64-url-safe :as base64]
            [hypercrud.browser.pages.entity :as entity]
            [hypercrud.browser.pages.index :as index]
            [hypercrud.client.core :as hc]
            [hypercrud.ui.collection :as collection :refer [cj-grid cj-grid-graph-deps]]
            [hypercrud.ui.form :refer [form-dependencies]]))


(defn graph-dependencies [forms state page-rel-path]
  (match [(string/split page-rel-path "/")]
         [[metatype "query" q]] (cj-grid-graph-deps ((keyword metatype) forms) (base64/decode q))
         [[metatype "entity" eid]] (form-dependencies eid ((keyword metatype) forms))
         [[""]] {}
         :else {}))


(defn ui [cur transact! graph forms index-queries page-rel-path]
  (let [cmd-chan (chan)
        commands (merge index/commands entity/commands)]
    (go
      (while true
        (let [cmd (<! cmd-chan)
              cmd-fn (get commands (first cmd))
              cmd-args (rest cmd)]
          (println (str "Cmd: " (name (first cmd))))
          (apply cmd-fn cmd-args))))
    (fn [cur transact! graph forms index-queries page-rel-path]
      [:div
       [:div.hc-node-view
        (match [(string/split page-rel-path "/")]
               [[metatype "query" q]]  [cj-grid graph forms (hc/select graph ::collection/query) (keyword metatype)]
               [[metatype "entity" eid]] (entity/view cur transact! graph (keyword metatype) forms cmd-chan (js/parseInt eid 10))
               [[""]] (index/view index-queries)
               :else [:div "no route for: " page-rel-path])]
       [:hr]
       [:pre (with-out-str (pprint/pprint @cur))]])))
