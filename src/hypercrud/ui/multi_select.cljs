(ns hypercrud.ui.multi-select
  (:require [hypercrud.ui.auto-control :refer [auto-control]]))


(defmulti multi-select-markup (fn [click-add! control-tuples] :default))


(defn multi-select* [markupfn value add-item! {:keys [change! field] :as widget-args}]
  (let [control-tuples (seq (mapv (fn [eid]
                                    (let [click-remove! #(change! [eid] nil)
                                          new-args (-> widget-args
                                                       (assoc-in [:field :attribute/cardinality] :db.cardinality/one)
                                                       (update :expanded-cur #(% [eid (:attribute/ident field)] {})))
                                          control [auto-control eid new-args]]
                                      [eid click-remove! control]))
                                  value))]
    (markupfn add-item! control-tuples)))


(defmethod multi-select-markup :default [click-add! control-tuples & [css-class]]
  [:div.multi-select {:class css-class}
   (map (fn [[eid click-remove! control]]
          ^{:key (str eid)}                                 ;(str eid) so this works when eid is nil
          [:div.multi-select-group
           [:button {:on-click click-remove!} "-"]
           control])
        control-tuples)
   [:button {:on-click click-add!} "+"]])
