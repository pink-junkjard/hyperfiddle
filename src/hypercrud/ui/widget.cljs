(ns hypercrud.ui.widget
  (:require [cljs.reader :as reader]
            [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx-util]
            [hypercrud.form.option :as option]
            [hypercrud.ui.auto-control :refer [auto-control]]
            [hypercrud.ui.code-editor :refer [code-editor*]]
            [hypercrud.ui.form :as form]
            [hypercrud.ui.input :as input]
            [hypercrud.ui.master-detail :refer [master-detail*]]
            [hypercrud.ui.multi-select :refer [multi-select* multi-select-markup]]
            [hypercrud.ui.radio :as radio]
            [hypercrud.ui.select :refer [select*]]
            [hypercrud.ui.table :as table]
            [hypercrud.ui.textarea :refer [textarea*]]
            [reagent.core :as r]))


(defn input-keyword [entity {:keys [field stage-tx!]}]
  (let [{:keys [:attribute/ident] :as attribute} (:field/attribute field)
        value (get entity ident)
        on-change! #(stage-tx! (tx-util/update-entity-attr entity attribute %))
        parse-string reader/read-string
        to-string str
        valid? #(try (let [code (reader/read-string %)]
                       (or (nil? code) (keyword? code)))
                     (catch :default e false))]
    [input/validated-input value on-change! parse-string to-string valid?]))


(defn input [entity {:keys [field stage-tx!]}]
  (let [{:keys [:attribute/ident] :as attribute} (:field/attribute field)
        value (get entity ident)
        on-change! #(stage-tx! (tx-util/update-entity-attr entity attribute %))]
    [input/input* value on-change!]))


(defn textarea [entity {:keys [field stage-tx!]}]
  (let [{:keys [:attribute/ident] :as attribute} (:field/attribute field)
        value (get entity ident)
        set-attr! #(stage-tx! (tx-util/update-entity-attr entity attribute %))]
    [textarea* {:type "text"
                :value value
                :on-change set-attr!}]))


(defn radio-ref [entity widget-args]
  ;;radio* needs parameterized markup fn todo
  [radio/radio-ref* entity widget-args])


(defn select-ref [entity {:keys [expanded-cur field] :as widget-args}]
  ;;select* has parameterized markup fn todo
  [select* entity widget-args
   [:button.edit {:on-click #(reset! expanded-cur {})
                  :disabled (nil? (get entity (-> field :field/attribute :attribute/ident)))} "Edit"]])


(defn select-ref-component [entity {:keys [expanded-cur field graph navigate-cmp stage-tx!]}]
  (let [value (get entity (-> field :field/attribute :attribute/ident))
        options (option/gimme-useful-options field)]
    (form/form graph value (option/get-form options entity) expanded-cur stage-tx! navigate-cmp)))


(defn table-many-ref [entity {:keys [field graph]}]
  (let [options (option/gimme-useful-options field)
        initial-select (first (option/get-option-records options graph entity))
        select-value-atom (r/atom (:db/id initial-select))]
    (fn [entity {:keys [field graph expanded-cur navigate-cmp stage-tx!]}]
      (let [options (option/gimme-useful-options field)
            ident (-> field :field/attribute :attribute/ident)
            entities (get entity ident)
            retract-entity! #(stage-tx! (tx-util/edit-entity (:db/id entity) ident [%] []))
            add-entity! #(stage-tx! (tx-util/edit-entity (:db/id entity) ident [] [%]))]
        [:div.value
         [table/table-managed graph entities (-> entity .-dbgraph .-dbval) (option/get-form options entity) expanded-cur stage-tx! navigate-cmp retract-entity! add-entity!]
         (let [props {:value (str @select-value-atom)
                      :on-change #(let [select-value (.-target.value %)
                                        value (reader/read-string select-value)]
                                   (reset! select-value-atom value))}
               ; todo assert selected value is in record set
               ; need lower level select component that can be reused here and in select.cljs
               select-options (->> (option/get-option-records options graph entity)
                                   (sort-by #(get % (option/label-prop options)))
                                   (map (fn [entity]
                                          [:option {:key (hash (:db/id entity))
                                                    :value (pr-str (:db/id entity))}
                                           (get entity (option/label-prop options))])))]
           [:div.table-controls
            [:select props select-options]
            [:button {:on-click #(add-entity! @select-value-atom)} "⬆"]])]))))


(defn table-many-ref-component [entity {:keys [field graph expanded-cur navigate-cmp stage-tx!]}]
  (let [ident (-> field :field/attribute :attribute/ident)
        options (option/gimme-useful-options field)
        entities (get entity ident)
        retract-entity! #(stage-tx! (tx-util/edit-entity (:db/id entity) ident [%] []))
        add-entity! #(stage-tx! (tx-util/edit-entity (:db/id entity) ident [] [%]))]
    [:div.value
     [table/table-managed graph entities (-> entity .-dbgraph .-dbval) (option/get-form options entity) expanded-cur stage-tx! navigate-cmp retract-entity! add-entity!]]))


(defn multi-select-ref [entity {:keys [field stage-tx!] :as widget-args}]
  (let [add-item! #(stage-tx! (tx-util/edit-entity (:db/id entity) (-> field :field/attribute :attribute/ident) [] [nil]))]
    (multi-select* multi-select-markup entity add-item! widget-args))) ;add-item! is: add nil to set


(defn multi-select-ref-component [entity {:keys [field stage-tx!] :as widget-args}]
  (let [temp-id! (partial hc/*temp-id!* (-> entity .-dbgraph .-dbval :conn-id)) ; bound to fix render bug
        add-item! #(stage-tx! (tx-util/edit-entity (:db/id entity) (-> field :field/attribute :attribute/ident) [] [(temp-id!)]))]
    [multi-select* multi-select-markup entity add-item! widget-args])) ;add new entity to set


(defn code-editor [entity {:keys [field stage-tx!]}]
  (let [ident (-> field :field/attribute :attribute/ident)
        value (get entity ident)
        change! #(stage-tx! (tx-util/edit-entity (:db/id entity) ident %1 %2))]
    ^{:key ident}
    [code-editor* value change!]))


(comment
  {:expanded
   {17592186045559 {:project/form {17592186045554 {:form/field {:field/attribute {}}}}}
    17592186045561 {:project/query {}}}})
; todo needs work with expanded-cur
#_(defn master-detail [entity {:keys [expanded-cur] :as widget-args}]
  (let [selected-atom (r/atom nil)]
    (fn [entity widget-args]
      (master-detail* entity widget-args selected-atom))))


(defn valid-date-str? [s]
  (or (empty? s)
      (let [ms (.parse js/Date s)]                          ; NaN if not valid string
        (integer? ms))))


(defn instant [entity {:keys [field stage-tx!]}]
  (let [{:keys [:attribute/ident] :as attribute} (:field/attribute field)
        value (get entity ident)
        on-change! #(stage-tx! (tx-util/update-entity-attr entity attribute %))
        parse-string (fn [s]
                       (if (empty? s)
                         nil
                         (let [ms (.parse js/Date s)]
                           (js/Date. ms))))
        to-string #(some-> % .toISOString)]
    [input/validated-input value on-change! parse-string to-string valid-date-str?]))


(defn default [field]
  (let [{:keys [:attribute/valueType :attribute/cardinality :attribute/isComponent]} (:field/attribute field)]
    [input/input*
     (str {:valueType (:db/ident valueType)
           :cardinality (:db/ident cardinality)
           :isComponent isComponent})
     #()
     {:read-only true}]))
