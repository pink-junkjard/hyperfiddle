(ns hypercrud.ui.select
  (:require [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx-util]
            [hypercrud.form.option :as option]
            [hypercrud.types :refer [->DbId]]
            [hypercrud.ui.form :as form]))


(defn select-boolean [entity {:keys [field stage-tx!]}]
  (let [ident (-> field :field/attribute :attribute/ident)
        value (get entity ident)
        props {;; normalize value for the dom - value is either nil, an :ident (keyword), or eid
               :value (if (nil? value) "" (str value))
               ;; reconstruct the typed value
               :on-change #(let [v (condp = (.-target.value %)
                                     "" nil
                                     "true" true
                                     "false" false)]
                            (stage-tx! (tx-util/update-entity-card-one-attr entity ident v)))}]
    [:div.value.editable-select {:key ident}
     [:span.select
      [:select props
       [:option {:key true :value "true"} "True"]
       [:option {:key false :value "false"} "False"]
       [:option {:key :nil :value ""} "--"]]]]))


(defn select* [entity {:keys [expanded-cur field graph navigate-cmp stage-tx!]} edit-element]
  (let [ident (-> field :field/attribute :attribute/ident)
        options (option/gimme-useful-options field)
        value (get entity ident)
        conn-id (-> entity .-dbval .-dbval .-conn-id)
        temp-id! (partial hc/*temp-id!* conn-id)
        props {;; normalize value for the dom - value is either nil, an :ident (keyword), or eid
               :value (cond
                        (nil? value) ""
                        (tx-util/tempid? value) "create-new"
                        :else (str value))

               ;; reconstruct the typed value
               :on-change #(do
                            (let [select-value (.-target.value %)
                                  dbid (cond
                                         (= "" select-value) nil
                                         (= "create-new" select-value) (temp-id!)
                                         :else-hc-select-option-node (->DbId (js/parseInt select-value 10) conn-id))]
                              ;reset the cursor before change! otherwise npe when trying to render
                              ;todo these both set the same cursor, and should be atomic
                              (reset! expanded-cur (if (= "create-new" select-value) {} nil))
                              (stage-tx! (tx-util/update-entity-card-one-attr entity ident dbid))
                              ;; and also add our new guy to the option list (for all combos)
                              ))}
        create-new? (some-> value tx-util/tempid?)
        show-form? (or (not= nil @expanded-cur) create-new?)]

    [:div.value.editable-select {:key (option/get-key options)}
     (if (and (option/editable? options entity) (not show-form?))
       edit-element)
     [:span.select
      (let [option-records (option/get-option-records options graph entity)]
        (assert (or (nil? value) (contains? (set option-records) value)) (str "Select options does not contain selected value: " (pr-str value)))
        [:select props (-> (->> option-records
                                (sort-by #(get % (option/label-prop options)))
                                (mapv (fn [entity]
                                        (let [dbid (:db/id entity)]
                                          ^{:key (hash dbid)}
                                          [:option {:value (.-id dbid)} (str (get entity (option/label-prop options)))]))))
                           (concat
                             (if (option/create-new? options entity)
                               [[:option {:key :create-new :value "create-new"} "Create New"]]
                               [])
                             [[:option {:key :blank :value ""} "--"]]))])]
     (if (and (not= nil value) show-form?)
       ;; TODO branch the client in create-new case
       [form/form graph value (option/get-form options entity) expanded-cur stage-tx! navigate-cmp])]))
