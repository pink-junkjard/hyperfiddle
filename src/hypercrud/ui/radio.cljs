(ns hypercrud.ui.radio
  (:require [hypercrud.client.tx :as tx]
            [hypercrud.form.option :as option]))


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
        change! #(stage-tx! (tx/update-entity-attr entity attribute %))]
    [:div.value.radio-boolean {:key ident}
     ^{:key :true}
     [radio-option "True" form-name #(change! true) (= true value)]
     ^{:key :false}
     [radio-option "False" form-name #(change! false) (= false value)]
     ^{:key :blank}
     [radio-option "--" form-name #(change! nil) (= nil value)]]))


(defn radio-ref* [entity {:keys [field graph stage-tx!]}]
  ; TODO only one radio-group on the page until we get a unique form-name
  (let [{:keys [:attribute/ident] :as attribute} (:field/attribute field)
        value (get entity ident)
        form-name "TODO"                                    ;form-name in the HTML sense
        change! #(stage-tx! (tx/update-entity-attr entity attribute %))]
    [:div.value {:key (option/get-key field)}
     (map (fn [result]
            (assert (= 1 (count result)) "Cannot use multiple find-elements for an options-link")
            (let [{:keys [:db/id] :as entity} (first result)
                  label (option/label-prop field result)
                  checked? (= id value)]
              ^{:key (hash id)}
              [radio-option label form-name #(change! id) checked?]))
          (option/get-option-records field graph entity))
     ^{:key :blank}
     [radio-option "--" form-name #(change! nil) (= nil value)]]))

(comment
  ; todo factor out aggregation into widget lib
  #_(defn radio-aggregate [fieldinfo eid stage-tx!]
      (let [selected-value (atom blog-value)]
        (fn [{:keys [name prompt values]} eid stage-tx!]
          [:div.field
           [:label prompt]
           (map (fn [{:keys [label value]}]
                  (let [change! #(let [rets (map (fn [[name val]] [:db/retract eid name val]) @selected-value)
                                       adds (map (fn [[name val]] [:db/add eid name val]) value)]
                                  (reset! selected-value value)
                                  (stage-tx! (vec (concat rets adds))))
                        checked? (= value @selected-value)]
                    ^{:key (hash [label value])}
                    [radio/radio-option label name change! checked?]))
                values)])))



  (defn radio-group [{:keys [name prompt values]} cur]
    [:div.field
     (if (not= nil prompt)
       [:label prompt])
     (map (fn [{:keys [label value]}]
            (let [checked? (= value @cur)]
              ^{:key (hash [label value])}
              [radio/radio-option label name #(reset! cur value) checked?]))
          values)]))
