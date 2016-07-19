(ns hypercrud.ui.collection
  (:require [hypercrud.client.core :as hc]
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
  [graph forms eid entity]
  (let [cols (map (fn [{:keys [name] :as fieldinfo}]
                    [:td.truncate {:key name}
                     [render-table-cell (some-> entity (get name))
                      fieldinfo
                      ;; the nav path uses the :name, :name doesn't
                      ;; match the lazy-cj-item's :rel in this case
                      {:graph graph :forms forms}]])
                  ((:meta/type entity) forms))]
    [:tr {:key eid}
     [:td {:key "edit-td"}
      [:a {:href (str "../entity/" eid)}
       "Edit"]]
     cols]))


;; :select :single or :select :multiple
;; the selection value is either the :href, or a set of :href
;; for now hardcode to single-select
;; Widgets can't take cursors directly, as there may be more complicated
;; state transitions desired.
(defn cj-grid [graph forms eids]
  [:table.u-full-width
   [:colgroup [:col {:span "1" :style {:width "20px"}}]]
   (cj-table-thead ((:meta/type (hc/entity graph (first eids))) forms))
   [:tbody
    (map (fn [eid]
           ^{:key eid}
           (cj-table-tr graph forms eid (hc/entity graph eid)))
         (take 10 eids))]])
