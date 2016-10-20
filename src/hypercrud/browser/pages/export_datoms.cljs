(ns hypercrud.browser.pages.export-datoms
  (:require [cljs.pprint :as pprint]
            [cljs.reader :as reader]
            [hypercrud.client.graph :as hc-g]
            [hypercrud.ui.code-editor :as code-editor]
            [promesa.core :as p]
            [reagent.core :as r]
            [hypercrud.client.tx :as tx]))


(def nope #{:db/ident
            :db/valueType
            :db/cardinality
            :db/doc
            :db/unique
            :db/index
            :db/fulltext
            :db/isComponent
            :db/noHistory
            :db/txInstant
            :fressian/tag})


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


(defn ->pulled-trees [pulled-trees-map]
  (apply concat (vals pulled-trees-map)))


(defn ->statements [schema pulled-trees-map]
  (->> (mapcat #(tx/pulled-tree-to-statements schema %)
               (->pulled-trees pulled-trees-map))
       set
       vec))


(defn ui [cur graph]
  (let [schema (hc-g/schema graph)
        datoms (->> (hc-g/graph-data graph)
                    :pulled-trees-map
                    (->statements schema)
                    (tx/replace-ids schema #(contains? nope %)))]
    [:div
     [:h2 "Project Datoms"]
     [:pre (with-out-str (pprint/pprint datoms))]])
  #_(let [query (cur [:query])]
      [:div
       [builder query]
       (if-let [results (:pulled-trees-map (hc-g/graph-data graph))]
         [:div
          [:hr]
          [:h2 "Results:"]
          [:pre (with-out-str (pprint/pprint results))]])]))


(defn query [state schema param-ctx]
  (let [dbval (get param-ctx :dbval)]
    (->> (keys schema)
         (remove #(contains? nope %))
         (remove nil?)
         (map (juxt identity (fn [ident] [[:find '[?e ...] :where ['?e ident]] [dbval] [dbval [:db/id ident]]])))
         (into {}))))
