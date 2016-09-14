(ns hypercrud.ui.table-cell
  (:require [hypercrud.client.core :as hc]
            [hypercrud.form.option :as option]))

(def ^:dynamic render-table-cell-dispatch
  (fn [val field props]
    (if (not (nil? val))
      (select-keys field [:valueType :cardinality])
      :default)))


(defmulti render-table-cell render-table-cell-dispatch)


(defmethod render-table-cell :default
  [val _ _]
  [:code {:key (pr-str val)} (pr-str val)])


(defmethod render-table-cell {:valueType :string :cardinality :db.cardinality/one}
  [val _ _]
  [:span {:key (pr-str val)} val])


(defmethod render-table-cell {:valueType :keyword :cardinality :db.cardinality/one}
  [val _ _]
  [:span {:key (pr-str val)} (name val)])


(defmethod render-table-cell {:valueType :keyword :cardinality :db.cardinality/many}
  [val _ _]
  [:span {:key (pr-str val)} (interpose ", " (map name val))])


(defmethod render-table-cell {:valueType :string :cardinality :db.cardinality/many}
  [val field props]
  [:span {:key (pr-str val)}
   (->> val
        (map #(render-table-cell % (assoc field :cardinality :db.cardinality/one) props))
        (interpose ", "))])


(defmethod render-table-cell {:valueType :ref :cardinality :db.cardinality/one}
  [eid {:keys [:options]} {:keys [graph forms] :as props}]
  (let [label-prop (option/label-prop options)
        form (option/get-form-id options "todo")]
    ^{:key eid}                                             ;symmetric with primitive-in-set
    [:a {:href (str "../../" form "/entity/" eid)}
     (render-table-cell (get (hc/entity graph eid) label-prop)
                        (first
                          (filter #(= (:name %) label-prop)
                                  (get forms form)))
                        props)]))

(defmethod render-table-cell {:valueType :ref :cardinality :db.cardinality/many}
  [val field props]
  [:span {:key (pr-str val)}
   (->> val
        (map #(render-table-cell % (assoc field :cardinality :db.cardinality/one) props))
        (interpose ", "))])

(defmethod render-table-cell {:valueType :instant :cardinality :db.cardinality/one}
  [val field props]
  [:span {:key (pr-str val)} (.toUTCString val)])