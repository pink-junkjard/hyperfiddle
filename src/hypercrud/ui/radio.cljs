(ns hypercrud.ui.radio
  (:require [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx-util]
            [hypercrud.form.option :as option]
            [hypercrud.ui.form :as form]))


(defn radio-option [label name change! checked?]
  ;; explicitly don't pass any arguments from react because the callsite
  ;; should have the value in closure scope, and the radios are confusing
  (let [change! #(change!)]
    [:div.radio-group
     [:input {:type "radio"
              :name name
              :checked checked?
              :on-change change!}]
     [:label {:on-click change!} label]]))


; value is a scalar, the choice of options which is set-scalar
; option is a set of scalar
;(defn radio-scalar [value options {:keys [name prompt values]} change!]
;  [:div.editable-radio {:key (hash options)}
;   (map (fn [{:keys [label value]}]
;          (let [change! nil
;                checked? (= value @cur)]
;            ^{:key (hash [label value])}
;            [radio-option label name change! checked?]))
;        options)
;   #_(comment
;       ^{:key :blank}
;       [radio-option "--" form-name #(change! nil) (= nil value)])])


(defn radio-boolean [entity {:keys [field stage-tx!]}]
  (let [{:keys [:attribute/ident] :as attribute} (:field/attribute field)
        value (get entity ident)
        form-name (str (:db/id entity) ident)
        change! #(stage-tx! (tx-util/update-entity-attr entity attribute %))]
    [:div.value.radio-boolean {:key ident}
     ^{:key :true}
     [radio-option "True" form-name #(change! true) (= true value)]
     ^{:key :false}
     [radio-option "False" form-name #(change! false) (= false value)]
     ^{:key :blank}
     [radio-option "--" form-name #(change! nil) (= nil value)]]))


(defn radio-ref* [entity {:keys [expanded-cur field graph navigate-cmp stage-tx!]}]
  ; TODO only one radio-group on the page until we get a unique form-name
  (let [{:keys [:attribute/ident] :as attribute} (:field/attribute field)
        options (option/gimme-useful-options field)
        value (get entity ident)
        form (option/get-form options entity)
        form-name (or (-> form :db/id .-id) "TODO")         ;form-name in the HTML sense
        temp-id! (partial hc/*temp-id!* (-> entity .-dbgraph .-dbval .-conn-id))
        change! (fn [dbid]
                  (let [dbid (if (= "create-new" dbid) (temp-id!) dbid)]
                    ;reset the cursor before change! otherwise npe when trying to render
                    ;todo these both set the same cursor, and should be atomic
                    (reset! expanded-cur (if (= "create-new" dbid) {} nil))
                    (stage-tx! (tx-util/update-entity-attr entity attribute dbid))))
        create-new? (some-> value tx-util/tempid?)
        show-form? (or (not= nil @expanded-cur) create-new?)]
    [:div.value {:key (option/get-key options)}
     (map (fn [{:keys [:db/id] :as entity}]
            (let [label (get entity (option/label-prop options))
                  checked? (= id value)]
              ^{:key (hash id)}
              [radio-option label form-name #(change! id) checked?]))
          (option/get-option-records options graph entity))
     (if (option/create-new? options entity)
       ^{:key :create-new}
       [radio-option "Create New" form-name #(change! "create-new") create-new?])
     ^{:key :blank}
     [radio-option "--" form-name #(change! nil) (= nil value)]
     (if show-form?
       ;; TODO branch the client in create-new case
       [form/form graph value form expanded-cur stage-tx! navigate-cmp])]

    ;todo how should editing existing entries work?
    #_[:div.editable-select {:key (hash option-eids)}
       (if (and form (not show-form?))
         [:button {:on-click #(swap! expanded-cur (constantly {}))
                   :disabled (= nil value)} "Edit"])
       ]))
