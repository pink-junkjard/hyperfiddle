(ns hypercrud.ui.table
  (:require [hypercrud.client.core :as hc]
            [hypercrud.ui.form-util :as form-util]
            [hypercrud.ui.table-cell :refer [render-table-cell]]))


(defn thead [form]
  (let [cols (map (fn [{:keys [:attribute/ident :field/prompt]}]
                    [:td {:key ident} prompt])
                  form)
        select-col [:td {:key "select-col"}]
        all-cols (conj cols select-col)]
    [:thead
     [:tr all-cols]]))


(defn tr [graph forms form-id eid entity]
  (let [cols (map (fn [{:keys [:attribute/ident] :as fieldinfo}]
                    [:td.truncate {:key ident}
                     [render-table-cell (some-> entity (get ident)) fieldinfo {:graph graph :forms forms}]])
                  (get forms form-id))]
    [:tr {:key eid}
     [:td {:key "edit-td"}
      [:a {:href (str "../entity/" eid)}
       "Edit"]]
     cols]))


(defn table [graph eids forms form-id]
  (let [form (get forms form-id)]
    [:table.u-full-width
     [:colgroup [:col {:span "1" :style {:width "20px"}}]]
     (thead form)
     [:tbody
      (let [entities (map #(hc/entity graph %) eids)
            ; hack todo we need sortkeys in our form construct
            sortkey (->> (map :attribute/ident form)
                         (filter #(-> % name (= "name")))
                         (first))]
        (->> (if sortkey
               (sort-by sortkey entities)
               entities)
             (take 10)
             (map (fn [entity]
                    (let [eid (:db/id entity)]
                      ^{:key eid}
                      (tr graph forms form-id eid entity))))))]]))


(defn query [q forms form-id expanded-forms]
  (form-util/query forms form-id expanded-forms {:query-name ::query
                                                 :query q
                                                 :params []}))
