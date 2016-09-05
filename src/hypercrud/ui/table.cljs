(ns hypercrud.ui.table
  (:require [hypercrud.client.core :as hc]
            [hypercrud.ui.table-cell :refer [render-table-cell]]))


(defn thead [form]
  (let [cols (map (fn [{:keys [name prompt]}]
                    [:td {:key name} prompt])
                  form)
        select-col [:td {:key "select-col"}]
        all-cols (conj cols select-col)]
    [:thead
     [:tr all-cols]]))


(defn tr [graph form-name forms eid entity]
  (let [cols (map (fn [{:keys [name] :as fieldinfo}]
                    [:td.truncate {:key name}
                     [render-table-cell (some-> entity (get name)) fieldinfo {:graph graph :forms forms}]])
                  (get forms form-name))]
    [:tr {:key eid}
     [:td {:key "edit-td"}
      [:a {:href (str "../entity/" eid)}
       "Edit"]]
     cols]))


(defn table [graph forms eids form-name]
  (let [form (get forms form-name)]
    [:table.u-full-width
     [:colgroup [:col {:span "1" :style {:width "20px"}}]]
     (thead form)
     [:tbody
      (let [entities (map #(hc/entity graph %) eids)
            ; hack todo we need sortkeys in our form construct
            sortkey (->> (map :name form)
                         (filter #(-> % name (= "name")))
                         (first))]
        (->> (if sortkey
               (sort-by sortkey entities)
               entities)
             (take 10)
             (map (fn [entity]
                    (let [eid (:db/id entity)]
                      ^{:key eid}
                      (tr graph form-name forms eid entity))))))]]))


(defn table-pull [form]
  (let [{:keys [refs notrefs]} (group-by (fn [{:keys [datatype]}]
                                           (if (= datatype :ref) :refs :notrefs))
                                         form)
        refpull (map (fn [field]
                       {(:name field) [:db/id (-> field :options :label-prop)]})
                     refs)
        non-refpull (map :name notrefs)]
    (concat refpull non-refpull [:db/id])))


(defn query [form q]
  {::query [q [] (table-pull form)]})
