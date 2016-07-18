(ns hypercrud.browser.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [<! chan]]
            [cljs.core.match :refer-macros [match]]
            [cljs.pprint :as pprint]
            [clojure.string :as string]
            [hypercrud.browser.pages.entity :as entity]
            [hypercrud.browser.pages.index :as index]
            [hypercrud.browser.pages.query :as query]
            [hypercrud.browser.base-64-url-safe :as base64]
            [hypercrud.client.core :as hc]))


;(defn query-query []
;  (let [form ((:meta/type))]))


(defn graph-dependencies [forms cur page-rel-path]
  (match [(string/split page-rel-path "/")]
         [["query" q]] {::query/query [(base64/decode q) [] '[*]]}
         [["entity" eid]] {::query/query ['[:find [?e ...] :in $ ?eid :where [?e :db/id ?eid]] [(js/parseInt eid 10)] '[*]]}))


(defn ui [cur client forms index-queries page-rel-path]
  (let [cmd-chan (chan)
        commands (merge index/commands entity/commands query/commands)]
    (go
      (while true
        (let [cmd (<! cmd-chan)
              cmd-fn (get commands (first cmd))
              cmd-args (rest cmd)]
          (println (str "Cmd: " (name (first cmd))))
          (apply cmd-fn cmd-args))))
    (fn [cur client forms index-queries page-rel-path]
      [:div
       [:div.hc-node-view
        (match [(string/split page-rel-path "/")]
               [["query" q]] (query/view q client forms)
               [["entity" eid]] (entity/view cur client forms cmd-chan (js/parseInt eid 10))
               [[""]] (index/view index-queries)
               :else [:div "no route for: " page-rel-path])]
       [:hr]
       [:pre (with-out-str (pprint/pprint @cur))]])))
