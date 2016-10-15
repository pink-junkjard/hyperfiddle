(ns hypercrud.browser.pages.hydrate
  (:require [cljs.pprint :as pprint]
            [cljs.reader :as reader]
            [hypercrud.client.graph :as hc-g]
            [hypercrud.ui.code-editor :as code-editor]
            [promesa.core :as p]
            [reagent.core :as r]))

(defn builder [cur]
  (let [query-stage (r/atom @cur)
        change! (fn [_ [new]]
                  (reset! query-stage new))]
    (fn [cur]
      [:div
       [:h2 "Query Builder:"]
       [code-editor/code-editor* @query-stage change!]
       [:button {:on-click #(reset! cur @query-stage)}
        "Submit"]])))


(defn ui [cur graph]
  (let [query (cur [:query])]
    [:div
     [builder query]
     (if-let [results (:pulled-trees-map (hc-g/graph-data graph))]
       [:div
        [:hr]
        [:h2 "Results:"]
        [:pre (with-out-str (pprint/pprint results))]])]))


(defn query [state]
  (some-> (get state :query) reader/read-string))
