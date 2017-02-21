(ns hypercrud.ui.radio
  (:require [cats.monad.exception :as exception]
            [hypercrud.client.tx :as tx]
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


(defn radio-boolean [entity field props {:keys [user-swap!] :as param-ctx}]
  (assert false "todo readonly")
  (let [{:keys [:attribute/ident] :as attribute} (:field/attribute field)
        value (get entity ident)
        form-name (str (:db/id entity) ident)
        change! #(user-swap! {:tx (tx/update-entity-attr entity attribute %)})]
    [:div.value.radio-boolean {:key ident}
     ^{:key :true}
     [radio-option "True" form-name #(change! true) (= true value)]
     ^{:key :false}
     [radio-option "False" form-name #(change! false) (= false value)]
     ^{:key :blank}
     [radio-option "--" form-name #(change! nil) (= nil value)]]))


(defn radio-ref* [value maybe-field props {:keys [user-swap!] :as param-ctx}]
  (assert false "todo readonly")
  ; TODO only one radio-group on the page until we get a unique form-name
  (let [form-name "TODO"                                    ;form-name in the HTML sense
        change! #(user-swap! {:tx (tx/update-entity-attr (:entity param-ctx) (:attribute param-ctx) %)})]
    [:div.value {:key (option/get-hydrate-key maybe-field)}
     (let [options (if maybe-field (option/hydrate-options maybe-field param-ctx) (exception/success []))]
       (if (exception/failure? options)
         [:span (pr-str (.-e options))])
       (map (fn [[dbid label]]
              ^{:key dbid} [radio-option label form-name #(change! dbid) (= dbid value)])
            (.-v options)))
     ^{:key :blank}
     [radio-option "--" form-name #(change! nil) (= nil value)]]))

(comment
  ; todo factor out aggregation into widget lib
  #_(defn radio-aggregate [fieldinfo eid user-swap!]
      (let [selected-value (atom blog-value)]
        (fn [{:keys [name prompt values]} eid user-swap!]
          [:div.field
           [:label prompt]
           (map (fn [{:keys [label value]}]
                  (let [change! #(let [rets (map (fn [[name val]] [:db/retract eid name val]) @selected-value)
                                       adds (map (fn [[name val]] [:db/add eid name val]) value)]
                                  (reset! selected-value value)
                                  (user-swap! {:tx (vec (concat rets adds))}))
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
