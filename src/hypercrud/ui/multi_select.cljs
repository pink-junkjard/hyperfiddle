(ns hypercrud.ui.multi-select
  (:require [hypercrud.ui.auto-control :refer [auto-control]]))


(defmulti multi-select-markup (fn [click-add! control-tuples] :default))


(defn multi-select* [markupfn fieldinfo graph metatype forms value change! add-item! transact! tempid!]
  (let [control-tuples (map (fn [v]
                              (let [click-remove! #(change! [:db/retract v])
                                    control [auto-control (assoc fieldinfo :set false) graph metatype forms
                                             v
                                             change!
                                             transact!
                                             tempid!]]
                                [v click-remove! control]))
                            value)]
    (markupfn add-item! control-tuples)))


(defmethod multi-select-markup :default [click-add! control-tuples & [css-class]]
  [:div.multi-select {:class css-class}
   (map (fn [[v click-remove! control]]
          ^{:key (str v)}                                   ;(str v) so this works when v is nil
          [:div.multi-select-group
           [:button {:on-click click-remove!} "-"]
           control])
        control-tuples)
   [:button {:on-click click-add!} "+"]])
