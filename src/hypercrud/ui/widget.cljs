(ns hypercrud.ui.widget
  (:require [cljs.reader :as reader]
            [hypercrud.client.core :as hc]
            [hypercrud.ui.auto-control :refer [auto-control]]
            [hypercrud.ui.code-editor :refer [code-editor*]]
            [hypercrud.ui.form :as form]
            [hypercrud.ui.input :as input]
            [hypercrud.ui.multi-select :refer [multi-select* multi-select-markup]]
            [hypercrud.ui.radio :refer [radio*]]
            [hypercrud.ui.select :refer [select*]]
            [hypercrud.ui.textarea :refer [textarea*]]))

(defn input-keyword [value change!]
  (let [parse-string keyword
        to-string #(subs (str %) 1)
        valid? (fn [s]
                 (let [kw (keyword s)
                       int? #(integer? (js/parseInt % 10))]
                   (and
                     (not (int? (subs (name kw) 0 1)))
                     (not (int? (subs (namespace kw) 0 1))))))]
    [input/validated-input value change! parse-string to-string valid?]))


(defn input [value change!]
  [input/input* {:type "text"
                 :value value
                 :on-change #(change! [value] [%])}])


(defn textarea [value change!]
  [textarea* {:type "text"
              :value value
              :on-change change!}])


(defn radio-ref [value widget-args]
  ;;radio* needs parameterized markup fn todo
  [radio* value widget-args])


(defn select-ref [value {:keys [expanded-cur] :as widget-args}]
  ;;select* has parameterized markup fn todo
  (let [ident (get-in widget-args [:field :attribute/ident])]
    [select* value (assoc widget-args :expanded-cur (expanded-cur [ident]))]))


(defn select-ref-component [value {:keys [expanded-cur field forms graph local-transact!]}]
  (form/form graph value forms (:field/form field) expanded-cur local-transact!))


(defn multi-select-ref [value {:keys [change!] :as widget-args}]
  (multi-select* multi-select-markup value #(change! [] [nil]) widget-args)) ;add-item! is: add nil to set


(defn multi-select-ref-component [value {:keys [change!] :as widget-args}]
  (let [temp-id! hc/*temp-id!*]
    [multi-select* multi-select-markup value #(change! [] [(temp-id!)]) widget-args])) ;add new entity to set


(defn code-editor [field value change!]
  ^{:key (:attribute/ident field)}
  [code-editor* value change!])


(defn valid-date-str? [s]
  (let [ms (.parse js/Date s)]                              ; NaN if not valid string
    (integer? ms)))


(defn instant [value change!]
  (let [parse-string #(let [ms (.parse js/Date %)]
                       (js/Date. ms))
        to-string #(some-> % .toISOString)]
    [input/validated-input value change! parse-string to-string valid-date-str?]))


(defn default [field]
  [input/input* {:type "text"
                 :value (str (select-keys field [:attribute/valueType :attribute/cardinality :attribute/isComponent]))
                 :read-only true}])
