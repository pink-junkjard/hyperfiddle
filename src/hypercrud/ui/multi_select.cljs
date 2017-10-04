(ns hypercrud.ui.multi-select
  (:require [hypercrud.browser.context :as context]
            [hypercrud.client.tx :as tx]
            [hypercrud.ui.auto-control :refer [auto-control]]))


(defmulti multi-select-markup (fn [click-add! control-tuples] :default))

(defn multi-select* [markupfn add-item! field anchors props {:keys [user-with!] :as param-ctx}]
  (assert false "todo readonly and test this")
  (let [control-tuples (seq (mapv (fn [inner-value]
                                    (let [click-remove! #(user-with! (tx/edit-entity (:db/id (:entity param-ctx)) (:attribute param-ctx) [inner-value] nil))
                                          param-ctx (-> param-ctx
                                                        (context/value inner-value)
                                                        (update-in [:attribute :db/cardinality] :db.cardinality/one))
                                          control [auto-control field anchors param-ctx]]
                                      [inner-value click-remove! control]))
                                  (:value param-ctx)))]
    (markupfn add-item! control-tuples)))

(defmethod multi-select-markup :default [click-add! control-tuples & [css-class]]
  [:div.value {:class css-class}
   (map (fn [[eid click-remove! control]]
          ^{:key (str eid)}                                 ;(str eid) so this works when eid is nil
          [:div.multi-select-group
           [:button {:on-click click-remove!} "-"]
           control])
        control-tuples)
   [:button {:on-click click-add!} "+"]])
