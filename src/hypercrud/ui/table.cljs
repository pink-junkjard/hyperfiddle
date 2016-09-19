(ns hypercrud.ui.table
  (:require [hypercrud.client.core :as hc]
            [hypercrud.form.util :as form-util]
            [hypercrud.ui.auto-control :refer [auto-table-cell]]))


(defn thead [form]
  (let [cols (map (fn [{:keys [:ident :prompt]}]
                    [:td {:key ident} prompt])
                  form)
        select-col [:td {:key "select-col"}]
        all-cols (conj cols select-col)]
    [:thead
     [:tr all-cols]]))


(defn tr [graph forms form-id eid entity expanded-cur stage-tx! navigate-cmp]
  (let [cols (map (fn [{:keys [:ident] :as field}]
                    [:td.truncate {:key ident}
                     [auto-table-cell entity form-id {:expanded-cur expanded-cur
                                                      :field field
                                                      :forms forms
                                                      :graph graph
                                                      :navigate-cmp navigate-cmp
                                                      :stage-tx! stage-tx!}]])
                  (get forms form-id))]
    [:tr {:key eid}
     [:td {:key "edit-td"}
      [navigate-cmp {:href (str form-id "/entity/" eid)}
       "Edit"]]
     cols]))


(defn table [graph eids forms form-id expanded-cur stage-tx! navigate-cmp]
  (let [form (get forms form-id)]
    [:table.hc-table
     [:colgroup [:col {:span "1" :style {:width "20px"}}]]
     (thead form)
     [:tbody
      (let [entities (map #(hc/entity graph %) eids)
            ; hack todo we need sortkeys in our form construct
            sortkey (->> (map :ident form)
                         (filter #(-> % name (= "name")))
                         (first))]
        (->> (if sortkey
               (sort-by sortkey entities)
               entities)
             (map (fn [entity]
                    (let [eid (:db/id entity)]
                      ^{:key eid}
                      (tr graph forms form-id eid entity (expanded-cur [eid]) stage-tx! navigate-cmp))))))]]))


(defn query [q forms form-id expanded-forms]
  (form-util/query forms form-id expanded-forms {:query-name ::query
                                                 :query q
                                                 :params []}))
