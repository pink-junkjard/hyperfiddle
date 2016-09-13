(ns hypercrud.ui.widget
  (:require [hypercrud.client.core :as hc]
            [hypercrud.ui.auto-control :refer [auto-control]]
            [hypercrud.ui.code-editor :refer [code-editor*]]
            [hypercrud.ui.form :as form]
            [hypercrud.ui.form-util :as form-util]
            [hypercrud.ui.input :as input]
            [hypercrud.ui.multi-select :refer [multi-select* multi-select-markup]]
            [hypercrud.ui.radio :as radio]
            [hypercrud.ui.select :refer [select*]]
            [hypercrud.ui.textarea :refer [textarea*]]))


(defn input-keyword [entity {:keys [local-transact!] {:keys [:attribute/ident]} :field}]
  (let [value (get entity ident)
        change! (partial form-util/change! local-transact! (:db/id entity) ident)
        parse-string keyword
        to-string #(subs (str %) 1)
        valid? (fn [s]
                 (let [kw (keyword s)
                       int? #(integer? (js/parseInt % 10))
                       safe-first (fn [s] (if (seq s) (subs s 0 1)))]
                   (and
                     (not (int? (safe-first (name kw))))
                     (not (int? (safe-first (namespace kw)))))))]
    [input/validated-input value change! parse-string to-string valid?]))


(defn input [entity {:keys [local-transact!] {:keys [:attribute/ident]} :field}]
  (let [value (get entity ident)
        change! #(form-util/change! local-transact! (:db/id entity) ident [value] [%])]
    [input/input* {:type "text"
                   :value value
                   :on-change change!}]))


(defn textarea [entity {:keys [local-transact!] {:keys [:attribute/ident]} :field}]
  (let [value (get entity ident)
        change! #(form-util/change! local-transact! (:db/id entity) ident [value] [%])]
    [textarea* {:type "text"
                :value value
                :on-change change!}]))


(defn radio-ref [entity widget-args]
  ;;radio* needs parameterized markup fn todo
  [radio/radio-ref* entity widget-args])


(defn select-ref [entity {:keys [expanded-cur] {:keys [:attribute/ident]} :field :as widget-args}]
  ;;select* has parameterized markup fn todo
  [select* entity (assoc widget-args :expanded-cur (expanded-cur [ident]))])


(defn select-ref-component [entity {:keys [expanded-cur field forms graph local-transact!]
                                    {:keys [:attribute/ident]} :field}]
  (let [value (get entity ident)]
    (form/form graph value forms (:field/form field) expanded-cur local-transact!)))


(defn multi-select-ref [entity {:keys [local-transact!] {:keys [:attribute/ident]} :field :as widget-args}]
  (let [add-item! #(form-util/change! local-transact! (:db/id entity) ident [] [nil])]
    (multi-select* multi-select-markup entity add-item! widget-args))) ;add-item! is: add nil to set


(defn multi-select-ref-component [entity {:keys [local-transact!] {:keys [:attribute/ident]} :field :as widget-args}]
  (let [temp-id! hc/*temp-id!*                              ; bound to fix render bug
        add-item! #(form-util/change! local-transact! (:db/id entity) ident [] [(temp-id!)])]
    [multi-select* multi-select-markup entity add-item! widget-args])) ;add new entity to set


(defn code-editor [entity {:keys [local-transact!] {:keys [:attribute/ident]} :field :as widget-args}]
  (let [value (get entity ident)
        change! (partial form-util/change! local-transact! (:db/id entity) ident)]
    ^{:key ident}
    [code-editor* value change!]))


(defn valid-date-str? [s]
  (let [ms (.parse js/Date s)]                              ; NaN if not valid string
    (integer? ms)))


(defn instant [entity {:keys [local-transact!]
                       {:keys [:attribute/ident]} :field :as widget-args}]
  (let [value (get entity ident)
        change! (partial form-util/change! local-transact! (:db/id entity) ident)
        parse-string #(let [ms (.parse js/Date %)]
                       (js/Date. ms))
        to-string #(some-> % .toISOString)]
    [input/validated-input value change! parse-string to-string valid-date-str?]))


(defn default [field]
  [input/input* {:type "text"
                 :value (str (select-keys field [:attribute/valueType :attribute/cardinality :attribute/isComponent]))
                 :read-only true}])
