(ns hypercrud.ui.collection
  (:require [hypercrud.client.core :as hypercrud]
            [hypercrud.ui.table-cell :refer [render-table-cell]]))


(defn cj-table-thead
  "Build table column headers, the cj-item may not be resolved"
  [form]
  (let [cols (map (fn [{:keys [name prompt]}]
                    [:td {:key name} prompt])
                  form)
        select-col [:td {:key "select-col"}]
        all-cols (conj cols select-col)]
    [:thead
     [:tr all-cols]]))


(defn cj-table-tr
  "Build a table row for the cj-item, the cj-item may not be resolved"
  [client forms eid entity]
  (let [cols (map (fn [{:keys [name] :as fieldinfo}]
                    [:td.truncate {:key name}
                     [render-table-cell (some-> entity (get name))
                      fieldinfo
                      ;; the nav path uses the :name, :name doesn't
                      ;; match the lazy-cj-item's :rel in this case
                      {:client client :forms forms}]])
                  ((:meta/type entity) forms))]
    [:tr {:key eid}
     [:td {:key "edit-td"}
      [:a {:href (str "/entity/" eid)}
       "Edit"]]
     cols]))


;; :select :single or :select :multiple
;; the selection value is either the :href, or a set of :href
;; for now hardcode to single-select
;; Widgets can't take cursors directly, as there may be more complicated
;; state transitions desired.
(defn cj-grid [client forms eids]
  [:table.u-full-width
   [:colgroup [:col {:span "1" :style {:width "20px"}}]]

   [hypercrud/resolve client (first eids)
    #(cj-table-thead ((:meta/type %) forms))
    (fn [entity] [:thead [:tr]])]

   [:tbody
    (map (fn [eid]
           ;; use the href as the radio name - hack
           ^{:key eid}
           [hypercrud/resolve client eid
            #(cj-table-tr client forms eid %)
            (fn [entity] [:tr])])
         (take 10 eids))]])