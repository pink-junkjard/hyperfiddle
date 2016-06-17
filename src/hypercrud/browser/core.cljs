(ns hypercrud.browser.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [<! chan]]
            [cljs.core.match :refer-macros [match]]
            [cljs.pprint :as pprint]
            [clojure.string :as string]
            [hypercrud.browser.pages.entity :as entity]
            [hypercrud.browser.pages.index :as index]
            [hypercrud.browser.pages.query :as query]
            [hypercrud.client.reagent :as hcr]))


(defn browser [cur client forms index-queries page-rel-path]
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
        (hcr/enter client
          (fn [tx]
            (match (string/split page-rel-path "/")
                   ["query" q] (query/view q client forms)
                   ["entity" eid] (entity/view cur client forms cmd-chan eid)
                   [& _] (index/view index-queries))))]
       [:hr]
       [:pre (with-out-str (pprint/pprint @cur))]])))
