(ns hypercrud.ui.select
  (:require [hypercrud.client.tx :as tx]
            [hypercrud.form.option :as option]
            [hypercrud.types :refer [->DbId]]))


(defn select-boolean [entity field links {:keys [user-swap!] :as param-ctx}]
  (let [{:keys [:attribute/ident] :as attribute} (:field/attribute field)
        value (get entity ident)
        props {;; normalize value for the dom - value is either nil, an :ident (keyword), or eid
               :value (if (nil? value) "" (str value))
               ;; reconstruct the typed value
               :on-change #(let [v (condp = (.-target.value %)
                                     "" nil
                                     "true" true
                                     "false" false)]
                             (user-swap! {:tx (tx/update-entity-attr entity attribute v)}))}]
    [:div.value.editable-select {:key ident}
     [:span.select
      [:select props
       [:option {:key true :value "true"} "True"]
       [:option {:key false :value "false"} "False"]
       [:option {:key :nil :value ""} "--"]]]]))


(defn select* [entity field {:keys [user-swap!] :as param-ctx}]
  (let [{:keys [:attribute/ident] :as attribute} (:field/attribute field)
        value (get entity ident)
        conn-id (-> entity .-dbgraph .-dbval .-conn-id)
        props {;; normalize value for the dom - value is either nil, an :ident (keyword), or eid
               :value (cond
                        (nil? value) ""
                        :else (-> value .-dbid .-id str))

               ;; reconstruct the typed value
               :on-change #(do
                             (let [select-value (.-target.value %)
                                   dbid (cond
                                          (= "" select-value) nil
                                          :else-hc-select-option-node (->DbId (js/parseInt select-value 10) conn-id))]
                               (user-swap! {:tx (tx/update-entity-attr entity attribute dbid)})))}]
    (let [option-records (option/get-option-records field param-ctx)]
      #_(assert (or (nil? value)
                    (tx/tempid? (.-dbid value))
                    (nil? option-records)                   ; user hasn't picked the query yet but may be about to
                    (contains? (set option-records) value)) (str "Select options does not contain selected value: " (pr-str value)))
      [:select.select props (-> (->> option-records
                                     (mapv (fn [result]
                                             (assert (= 1 (count result)) "Cannot use multiple find-elements for an options-link")
                                             (let [entity (first result)
                                                   dbid (:db/id entity)
                                                   label-prop (option/label-prop field result)]
                                               [dbid label-prop])))
                                     (sort-by second)
                                     (mapv (fn [[dbid label-prop]]
                                             ^{:key (hash dbid)}
                                             [:option {:value (.-id dbid)} label-prop])))
                                (concat
                                  [[:option {:key :blank :value ""} "--"]]))])))
