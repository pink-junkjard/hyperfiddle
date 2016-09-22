(ns hypercrud.ui.table
  (:require [hypercrud.client.core :as hc]
            [hypercrud.form.util :as form-util]
            [hypercrud.ui.auto-control :refer [auto-table-cell]]
            [clojure.set :as set]))


(defn build-col-heads [form]
  (map (fn [{:keys [:ident :prompt]}]
         [:td {:key ident} prompt])
       form))


(defn build-row-cells [form-id entity {:keys [forms] :as fieldless-widget-args}]
  (map (fn [{:keys [ident] :as field}]
         [:td.truncate {:key ident}
          [auto-table-cell entity form-id (-> fieldless-widget-args
                                              (update :expanded-cur #(% [ident]))
                                              (assoc :field field))]])
       (get forms form-id)))


(defn table [graph eids forms form-id expanded-cur stage-tx! navigate-cmp retract-entity]
  (let [form (get forms form-id)]
    [:table.ui-table
     [:colgroup [:col {:span "1" :style {:width "20px"}}]]
     [:thead
      [:tr
       (if retract-entity [:td.remove-row {:key "remove-col"}])
       [:td {:key "select-col"}]
       (build-col-heads form)]]
     [:tbody
      (let [entities (map #(hc/entity graph %) eids)
            ; hack todo we need sortkeys in our form construct
            sortkey (->> (map :ident form)
                         (filter #(-> % name (= "name")))
                         (first))]
        (->> (if sortkey (sort-by sortkey entities) entities)
             (map (fn [{:keys [:db/id] :as entity}]
                    [:tr {:key id}
                     (if retract-entity
                       [:td.remove-row {:key "remove"}
                        [:button {:on-click #(retract-entity id)} "⌦"]])
                     [:td.id {:key "edit-td"}
                      [navigate-cmp {:href (str form-id "/entity/" id)} (if (neg? id) id (mod id 100))]]
                     (build-row-cells form-id entity {:expanded-cur (expanded-cur [id])
                                                      :forms forms
                                                      :graph graph
                                                      :navigate-cmp navigate-cmp
                                                      :stage-tx! stage-tx!})]))))]]))


(defn query [query-blob forms form-id expanded-forms]
  (let [q (:q query-blob)
        hp (:hp query-blob)
        hole-names (form-util/parse-holes q)
        _ (assert (set/subset? (set hole-names) (set (keys hp)))
                  (str "Missing parameters: " (set/difference (set hole-names) (set (keys hp))) " for query: " q))
        params (map #(get hp %) hole-names)]
    (form-util/query forms form-id expanded-forms {:query-name ::query
                                                   :query q
                                                   :params params})))