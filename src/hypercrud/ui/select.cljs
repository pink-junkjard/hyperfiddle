(ns hypercrud.ui.select
  (:require [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx-util]
            [hypercrud.form.option :as option]
            [hypercrud.types :as types]
            [hypercrud.ui.form :as form]))


(defn select-boolean [entity {:keys [stage-tx!] {:keys [ident]} :field}]
  (let [value (get entity ident)
        props {;; normalize value for the dom - value is either nil, an :ident (keyword), or eid
               :value (if (nil? value) "" (str value))
               ;; reconstruct the typed value
               :on-change #(let [v (condp = (.-target.value %)
                                     "" nil
                                     "true" true
                                     "false" false)]
                            (stage-tx! (tx-util/update-entity-attr entity ident v)))}]
    [:div.value.editable-select {:key ident}
     [:span.select
      [:select props
       [:option {:key true :value "true"} "True"]
       [:option {:key false :value "false"} "False"]
       [:option {:key :nil :value ""} "--"]]]]))


(defn select* [entity {:keys [expanded-cur forms graph navigate-cmp queries stage-tx!]
                       {:keys [ident options]} :field} edit-element]
  (let [value (get entity ident)
        dbval (-> entity meta :dbval)
        temp-id! (partial hc/*temp-id!* (:conn-id dbval))
        props {;; normalize value for the dom - value is either nil, an :ident (keyword), or eid
               :value (cond
                        (nil? value) ""
                        (tx-util/tempid? value) "create-new"
                        :else (str value))

               ;; reconstruct the typed value
               :on-change #(do
                            (let [select-value (.-target.value %)
                                  eid (cond
                                        (= "" select-value) nil
                                        (= "create-new" select-value) (temp-id!)
                                        :else-hc-select-option-node (types/->DbId (js/parseInt select-value 10) (:conn-id dbval)))]
                              ;reset the cursor before change! otherwise npe when trying to render
                              ;todo these both set the same cursor, and should be atomic
                              (reset! expanded-cur (if (= "create-new" select-value) {} nil))
                              (stage-tx! (tx-util/update-entity-attr entity ident eid))
                              ;; and also add our new guy to the option list (for all combos)
                              ))}
        create-new? (some-> value tx-util/tempid?)
        show-form? (or (not= nil @expanded-cur) create-new?)]

    [:div.value.editable-select {:key (option/get-key options queries)}
     (if (and (option/editable? options entity) (not show-form?))
       edit-element)
     [:span.select
      (let [option-records (option/get-option-records options queries graph entity)]
        (assert (or (nil? value) (contains? (set (map :db/id option-records)) value)) (str "Select options does not contain selected value: " (pr-str value)))
        [:select props (-> (->> option-records
                                (sort-by #(get % (option/label-prop options)))
                                (mapv (fn [entity]
                                        (let [dbid (:db/id entity)]
                                          ^{:key (hash dbid)}
                                          [:option {:value (:id dbid)} (str (get entity (option/label-prop options)))]))))
                           (concat
                             (if (option/create-new? options entity)
                               [[:option {:key :create-new :value "create-new"} "Create New"]]
                               [])
                             [[:option {:key :blank :value ""} "--"]]))])]
     (if (and (not= nil value) show-form?)
       ;; TODO branch the client in create-new case
       [form/form graph dbval value forms queries (option/get-form-id options entity) expanded-cur stage-tx! navigate-cmp])]))
