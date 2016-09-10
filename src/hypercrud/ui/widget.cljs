(ns hypercrud.ui.widget
  (:require [cljs.reader :as reader]
            [hypercrud.client.core :as hc]
            [hypercrud.ui.auto-control :refer [auto-control]]
            [hypercrud.ui.code-editor :refer [code-editor*]]
            [hypercrud.ui.form :as form]
            [hypercrud.ui.input :refer [input*]]
            [hypercrud.ui.multi-select :refer [multi-select* multi-select-markup]]
            [hypercrud.ui.radio :refer [radio*]]
            [hypercrud.ui.select :refer [select*]]
            [hypercrud.ui.textarea :refer [textarea*]]
            [reagent.core :as reagent]))

(defn input-keyword [value change!]
  [input* {:type "text"
           :value (pr-str value)
           :on-change #(change! [value] [(reader/read-string %)])}])

(defn input [value change!]
  [input* {:type "text"
           :value value
           :on-change #(change! [value] [%])}])


(defn textarea [value change!]
  [textarea* {:type "text"
              :value value
              :on-change change!}])


(defn radio-ref [value widget-args]
  ;;radio* needs parameterized markup fn todo
  [radio* value widget-args])


(defn select-ref [value widget-args]
  ;;select* has parameterized markup fn todo
  [select* value widget-args])


(defn select-ref-component [value {:keys [expanded-cur field forms graph local-transact!]}]
  (form/form graph value forms (:field/form field) expanded-cur local-transact!))


(defn multi-select-ref [value {:keys [change!] :as widget-args}]
  (multi-select* multi-select-markup value #(change! [] [nil]) widget-args)) ;add-item! is: add nil to set


(defn multi-select-ref-component [value {:keys [change! graph] :as widget-args}]
  [multi-select* multi-select-markup value #(change! [] [(hc/temp-id! graph)]) widget-args]) ;add new entity to set


(defn code-editor [field value change!]
  ^{:key (:attribute/ident field)}
  [code-editor* value change!])


(defn valid-date-str? [s]
  (let [ms (.parse js/Date s)]                              ; NaN if not valid string
    (integer? ms)))


(defn instant [value change!]
  (let [intermediate-val (reagent/atom (some-> value .toISOString))]
    (fn [value change!]
      [:input {:type "text"
               :class (if-not (valid-date-str? @intermediate-val) "invalid")
               :value @intermediate-val
               :on-change (fn [e]
                            (let [input-str (.. e -target -value)]
                              (reset! intermediate-val input-str) ;always save what they are typing
                              (if (valid-date-str? input-str)
                                (change! [value]
                                         [(let [ms (.parse js/Date input-str)] (js/Date. ms))]))))}])))


(defn default [field]
  [input* {:type "text"
           :value (str (select-keys field [:attribute/valueType :attribute/cardinality :attribute/isComponent]))
           :read-only true}])
