(ns hypercrud.ui.table
  (:require [hypercrud.client.core :as hc]
            [hypercrud.form.util :as form-util]
            [hypercrud.ui.auto-control :refer [auto-table-cell]]
            [hypercrud.form.q-util :as q-util]))


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


(defn table [graph eids forms queries form-id expanded-cur stage-tx! navigate-cmp retract-entity]
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
                                                      :queries queries
                                                      :stage-tx! stage-tx!})]))))]]))


(defn table-pull-exp [forms form]
  (concat
    [:db/id]
    (map (fn [{:keys [:ident :cardinality :isComponent :options] :as field}]
           (if (or isComponent (= cardinality :db.cardinality/many))
             ; components and selects render nested entities, so pull another level deeper
             (let [form (form-util/options->form forms options)]
               {ident (table-pull-exp forms form)})

             ; otherwise just add the attribute to the list
             ident))
         form)))


(defn query [q param-ctx forms form-id]
  (let [form (get forms form-id)
        pull-exp (table-pull-exp forms form)]
    (q-util/build-query ::query q param-ctx pull-exp)))