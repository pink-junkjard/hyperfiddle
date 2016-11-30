(ns hypercrud.ui.widget
  (:require [cljs.reader :as reader]
            [hypercrud.browser.links :as links]
            [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx]
            [hypercrud.form.option :as option]
            [hypercrud.ui.auto-control :refer [auto-control]]
            [hypercrud.ui.code-editor :refer [code-editor*]]
            [hypercrud.ui.input :as input]
            [hypercrud.ui.multi-select :refer [multi-select* multi-select-markup]]
            [hypercrud.ui.radio :as radio]
            [hypercrud.ui.select :refer [select*]]
            [hypercrud.ui.textarea :refer [textarea*]]
            [reagent.core :as r]))


(defn link-thing [{:keys [field links navigate-cmp param-ctx stage-tx!] :as widget-args}]
  (let [field-dbid (.-dbid field)]
    [:div.links
     (->> links
          ; we are assuming link/repeating? true
          ; should we? do we need buz logic to prevent that?
          (filter #(= field-dbid (some-> % :link/field .-dbid)))
          (map (fn [{:keys [:db/id :link/prompt] :as link}]
                 ^{:key id}
                 [navigate-cmp (links/query-link stage-tx! link param-ctx) prompt]))
          (interpose " · "))]))


(defn input-keyword [entity {:keys [field stage-tx!]}]
  (let [{:keys [:attribute/ident] :as attribute} (:field/attribute field)
        value (get entity ident)
        on-change! #(stage-tx! (tx/update-entity-attr entity attribute %))
        parse-string reader/read-string
        to-string str
        valid? #(try (let [code (reader/read-string %)]
                       (or (nil? code) (keyword? code)))
                     (catch :default e false))]
    [input/validated-input value on-change! parse-string to-string valid?]))


(defn input [entity {:keys [field stage-tx!]}]
  (let [{:keys [:attribute/ident] :as attribute} (:field/attribute field)
        value (get entity ident)
        on-change! #(stage-tx! (tx/update-entity-attr entity attribute %))]
    [input/input* value on-change!]))


(defn input-long [entity {:keys [field stage-tx!]}]
  (let [{:keys [:attribute/ident] :as attribute} (:field/attribute field)]
    [input/validated-input
     (get entity ident) #(stage-tx! (tx/update-entity-attr entity attribute %))
     #(js/parseInt % 10) pr-str
     #(integer? (js/parseInt % 10))]))


(defn textarea [entity {:keys [field stage-tx!]}]
  (let [{:keys [:attribute/ident] :as attribute} (:field/attribute field)
        value (get entity ident)
        set-attr! #(stage-tx! (tx/update-entity-attr entity attribute %))]
    [textarea* {:type "text"
                :value value
                :on-change set-attr!}]))


(defn radio-ref [entity widget-args]
  ;;radio* needs parameterized markup fn todo
  [radio/radio-ref* entity widget-args])


; this can be used sometimes, on the entity page, but not the query page
(defn select-ref [entity {:keys [field] :as widget-args}]
  [:div.value.editable-select {:key (option/get-key field)}
   [:span.select
    (select* entity widget-args)
    (link-thing widget-args)]])


(defn select-ref-component [entity {:keys [field graph navigate-cmp stage-tx!] :as widget-args}]
  [:div.value
   (link-thing widget-args)]
  #_(let [value (get entity (-> field :field/attribute :attribute/ident))]
    (form/form graph value (:field/form field) stage-tx! navigate-cmp)))


(defn table-many-ref [entity {:keys [field graph] :as widget-args}]
  [:div.value
   (link-thing widget-args)]
  #_(let [initial-select (let [result (first (option/get-option-records field graph entity))]
                         (assert (= 1 (count result)) "Cannot use multiple find-elements for an options-link")
                         (first result))
        select-value-atom (r/atom (:db/id initial-select))]
    (fn [entity {:keys [field graph navigate-cmp stage-tx!]}]
      (let [ident (-> field :field/attribute :attribute/ident)
            resultset (mapv vector (get entity ident))]
        [:div.value
         [table/table graph resultset (vector (:field/form field)) stage-tx! navigate-cmp]
         (let [props {:value (str @select-value-atom)
                      :on-change #(let [select-value (.-target.value %)
                                        value (reader/read-string select-value)]
                                   (reset! select-value-atom value))}
               ; todo assert selected value is in record set
               ; need lower level select component that can be reused here and in select.cljs
               select-options (->> (option/get-option-records field graph entity)
                                   (mapv (fn [result]
                                           (assert (= 1 (count result)) "Cannot use multiple find-elements for an options-link")
                                           (let [entity (first result)]
                                             [(:db/id entity) (option/label-prop field result)])))
                                   (sort-by second)
                                   (map (fn [[dbid label-prop]]
                                          [:option {:key (hash dbid) :value (pr-str dbid)} label-prop])))]
           [:div.table-controls
            [:select props select-options]
            [:button {:on-click #(stage-tx! (tx/edit-entity (:db/id entity) ident [] [@select-value-atom]))} "⬆"]])]))))


(defn table-many-ref-component [entity {:keys [field graph navigate-cmp stage-tx!] :as widget-args}]
  [:div.value
   (link-thing widget-args)]
  #_(let [ident (-> field :field/attribute :attribute/ident)
        resultset (map vector (get entity ident))]
    [:div.value
     [table/table graph resultset (vector (:field/form field)) stage-tx! navigate-cmp]]))


(defn multi-select-ref [entity {:keys [field stage-tx!] :as widget-args}]
  (let [add-item! #(stage-tx! (tx/edit-entity (:db/id entity) (-> field :field/attribute :attribute/ident) [] [nil]))]
    (multi-select* multi-select-markup entity add-item! widget-args))) ;add-item! is: add nil to set


(defn multi-select-ref-component [entity {:keys [field stage-tx!] :as widget-args}]
  (let [temp-id! (partial hc/*temp-id!* (-> entity .-dbgraph .-dbval :conn-id)) ; bound to fix render bug
        add-item! #(stage-tx! (tx/edit-entity (:db/id entity) (-> field :field/attribute :attribute/ident) [] [(temp-id!)]))]
    [multi-select* multi-select-markup entity add-item! widget-args])) ;add new entity to set


(defn code-editor [entity {:keys [field stage-tx!]}]
  (let [ident (-> field :field/attribute :attribute/ident)
        value (get entity ident)
        change! #(stage-tx! (tx/edit-entity (:db/id entity) ident [value] [%]))]
    ^{:key ident}
    [code-editor* value change!]))


(defn valid-date-str? [s]
  (or (empty? s)
      (let [ms (.parse js/Date s)]                          ; NaN if not valid string
        (integer? ms))))


(defn instant [entity {:keys [field stage-tx!]}]
  (let [{:keys [:attribute/ident] :as attribute} (:field/attribute field)
        value (get entity ident)
        on-change! #(stage-tx! (tx/update-entity-attr entity attribute %))
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
