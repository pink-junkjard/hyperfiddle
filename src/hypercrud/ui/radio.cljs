(ns hypercrud.ui.radio
  (:require [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx-util]
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

;(defprotocol IOptions
;  (key [this nil])
;  (get-options [this graph record])
;  (validdate-field?))
;
;(extend-type Keyword
;  IOptions
;  (key [this] nil)
;  (get-options [this graph record] nil))
;
;(extend-type Vector
;  IOptions
;  (get-options [this graph record]
;    (let [[q args] this]
;      (->> (hc/select graph (hash q) q)
;           (map #(hc/entity graph %))))))
;
;(extend-type IFn
;  IOptions
;  (get-options [this graph record] (this graph record)))
;
;
;{:attribute/ident :attribute/isComponent
; :field/prompt "Is Component?"
; :attribute/valueType :boolean
; :db/cardinality :db.cardinality/one
;
; :field/query {:options/query ['[:find [?e ...] :where [?e :form/name]] []]}
;
; :field/query {:options/inline (constantly [{:value true :label "true"}
;                                            {:value false :label "false"}
;                                            {:value nil :label "--"}])}
;
; :field/query {:options/self :studyitem/related-study-items}
;
;
; :field/label-prop :label}
;
;
;; value is a scalar, the choice of options which is set-scalar
;; option is a set of scalar
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

(defn radio-ref* [entity {:keys [expanded-cur forms graph stage-tx!]
                          {:keys [:field/label-prop :field/form :attribute/ident] {[query args] :query/value} :field/query} :field}]
  ; TODO only one radio-group on the page until we get a unique form-name
  (let [value (get entity ident)
        expanded-cur (expanded-cur [ident])
        form-name (or form "TODO")                          ;form-name in the HTML sense
        option-eids (hc/select graph (hash query) query)
        temp-id! hc/*temp-id!*
        change! (fn [eid]
                  (let [eid (if (= "create-new" eid) (temp-id!) eid)]
                    ;reset the cursor before change! otherwise npe when trying to render
                    ;todo these both set the same cursor, and should be atomic
                    (reset! expanded-cur (if (= "create-new" eid) {} nil))
                    (stage-tx! (tx-util/update-entity-attr entity ident eid))))
        create-new? (some-> value tx-util/tempid?)
        show-form? (or (not= nil @expanded-cur) create-new?)]
    [:div.editable-radio {:key (hash option-eids)}
     (map (fn [eid]
            (let [entity (hc/entity graph eid)
                  label (get entity label-prop)
                  checked? (= eid value)]
              ^{:key eid}
              [radio-option label form-name #(change! eid) checked?]))
          option-eids)
     (if form
       ^{:key :create-new}
       [radio-option "Create New" form-name #(change! "create-new") create-new?])
     ^{:key :blank}
     [radio-option "--" form-name #(change! nil) (= nil value)]
     (if show-form?
       ;; TODO branch the client in create-new case
       [form/form graph value forms form expanded-cur stage-tx!])]

    ;todo how should editing existing entries work?
    #_[:div.editable-select {:key (hash option-eids)}
       (if (and form (not show-form?))
         [:button {:on-click #(swap! expanded-cur (constantly {}))
                   :disabled (= nil value)} "Edit"])
       ]))
