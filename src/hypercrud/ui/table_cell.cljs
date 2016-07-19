(ns hypercrud.ui.table-cell
  (:require [hypercrud.client.core :as hc]))

(def ^:dynamic render-table-cell-dispatch
  (fn [val fieldinfo props]
    (if (not (nil? val))
      (select-keys fieldinfo [:datatype :set])
      :default)))


(defmulti render-table-cell render-table-cell-dispatch)


(defmethod render-table-cell :default
  [val _ _]
  [:code {:key (pr-str val)} (pr-str val)])

(defmethod render-table-cell {:datatype :string :set false}
  [val _ _]
  [:span {:key (pr-str val)} val])

(defmethod render-table-cell {:datatype :keyword :set false}
  [val _ _]
  [:span {:key (pr-str val)} (name val)])

(defmethod render-table-cell {:datatype :keyword :set true}
  [val _ _]
  [:span {:key (pr-str val)} (interpose ", " (map name val))])

(defmethod render-table-cell {:datatype :string :set true}
  [val fieldinfo props]
  [:span {:key (pr-str val)}
   (let [rendered-set-items (map #(render-table-cell % (assoc fieldinfo :set false) props)
                                 val)]
     (interpose ", " rendered-set-items))])

(defmethod render-table-cell {:datatype :ref :set false}
  [eid
   {{:keys [label-prop metatype]} :options}
   {:keys [graph forms] :as props}]
  (assert (not (nil? label-prop)))

  ^{:key eid}                                               ;symmetric with primitive-in-set
  [:a {:href (str "../../" (name metatype) "/entity/" eid)}
   (render-table-cell (get (hc/entity graph eid) label-prop)
                      (first
                        (filter #(= (:name %) label-prop)
                                (metatype forms)))
                      props)])

(defmethod render-table-cell {:datatype :ref :set true}
  [val fieldinfo props]
  [:span {:key (pr-str val)}
   (->>
     (map (fn [{:keys [rel] :as val}]
            (render-table-cell val (assoc fieldinfo :set false) props))
          val)
     (interpose ", "))])
