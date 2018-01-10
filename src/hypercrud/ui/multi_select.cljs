(ns hypercrud.ui.multi-select
  (:require [hypercrud.browser.context :as context]
            [hypercrud.client.tx :as tx]
            [hypercrud.ui.auto-control :refer [schema-control-form]]))


(defn multi-select* [markupfn add-item! field anchors props {:keys [user-with!] :as ctx}]
  (assert false "todo readonly and test this")
  (let [control-tuples (seq (mapv (fn [inner-value]
                                    (let [click-remove! #(user-with! (tx/edit-entity (-> ctx :cell-data deref :db/id) (:attribute ctx) [inner-value] nil))
                                          ctx (-> ctx
                                                  (context/value inner-value)
                                                  (update-in [:attribute :db/cardinality] :db.cardinality/one))
                                          control [schema-control-form field anchors ctx]]
                                      [inner-value click-remove! control]))
                                  @(:value ctx)))]
    (markupfn add-item! control-tuples)))

(defn multi-select-markup [click-add! control-tuples & [css-class]]
  [:div.value {:class css-class}
   (map (fn [[eid click-remove! control]]
          ^{:key (str eid)}                                 ;(str eid) so this works when eid is nil
          [:div.multi-select-group
           [:button {:on-click click-remove!} "-"]
           control])
        control-tuples)
   [:button {:on-click click-add!} "+"]])
