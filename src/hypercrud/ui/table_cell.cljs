(ns hypercrud.ui.table-cell
  (:require [hypercrud.client.core :as hc]))

(def ^:dynamic render-table-cell-dispatch
  (fn [val fieldinfo props]
    (if (not (nil? val))
      (select-keys fieldinfo [:attribute/valueType :attribute/cardinality])
      :default)))


(defmulti render-table-cell render-table-cell-dispatch)


(defmethod render-table-cell :default
  [val _ _]
  [:code {:key (pr-str val)} (pr-str val)])


(defmethod render-table-cell {:attribute/valueType :string :attribute/cardinality :db.cardinality/one}
  [val _ _]
  [:span {:key (pr-str val)} val])


(defmethod render-table-cell {:attribute/valueType :keyword :attribute/cardinality :db.cardinality/one}
  [val _ _]
  [:span {:key (pr-str val)} (name val)])


(defmethod render-table-cell {:attribute/valueType :keyword :attribute/cardinality :db.cardinality/many}
  [val _ _]
  [:span {:key (pr-str val)} (interpose ", " (map name val))])


(defmethod render-table-cell {:attribute/valueType :string :attribute/cardinality :db.cardinality/many}
  [val fieldinfo props]
  [:span {:key (pr-str val)}
   (let [rendered-set-items (map #(render-table-cell % (assoc fieldinfo :attribute/cardinality :db.cardinality/one) props)
                                 val)]
     (interpose ", " rendered-set-items))])


(defmethod render-table-cell {:attribute/valueType :ref :attribute/cardinality :db.cardinality/one}
  [eid
   {{:keys [:option/label-prop :option/form]} :field/options}
   {:keys [graph forms] :as props}]
  (assert (not (nil? label-prop)))

  ^{:key eid}                                               ;symmetric with primitive-in-set
  [:a {:href (str "../../" form "/entity/" eid)}
   (render-table-cell (get (hc/entity graph eid) label-prop)
                      (first
                        (filter #(= (:name %) label-prop)
                                (get forms form)))
                      props)])


(defmethod render-table-cell {:attribute/valueType :ref :attribute/cardinality :db.cardinality/many}
  [val fieldinfo props]
  [:span {:key (pr-str val)}
   (->>
     (map (fn [{:keys [rel] :as val}]
            (render-table-cell val (assoc fieldinfo :attribute/cardinality :db.cardinality/one) props))
          val)
     (interpose ", "))])

(defmethod render-table-cell {:attribute/valueType :instant :attribute/cardinality :db.cardinality/one}
  [val field props]
  [:span {:key (pr-str val)} (.toUTCString val)])