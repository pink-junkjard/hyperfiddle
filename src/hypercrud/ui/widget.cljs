(ns hypercrud.ui.widget
  (:require [hypercrud.ui.auto-control :refer [auto-control]]
            [hypercrud.ui.code-editor :refer [code-editor*]]
            [hypercrud.ui.form :refer [form]]
            [hypercrud.ui.input :refer [input*]]
            [hypercrud.ui.multi-select :refer [multi-select* multi-select-markup]]
            [hypercrud.ui.radio :refer [radio*]]
            [hypercrud.ui.select :refer [select*]]
            [hypercrud.ui.textarea :refer [textarea*]]
            [reagent.core :as reagent]))


(defn input [fieldinfo graph forms value expanded-cur change! transact! tempid!]
  [input* {:type "text"
           :value value
           :on-change change!}])


(defn textarea [fieldinfo graph forms value expanded-cur change! transact! tempid!]
  [textarea* {:type "text"
              :value value
              :on-change change!}])


(defn radio-ref [{:keys [name options] :as fieldinfo} graph forms value expanded-cur change! transact! tempid!]
  ;;radio* needs parameterized markup fn todo
  [radio* graph forms options value (expanded-cur [name]) change! transact! tempid!])


(defn select-ref [{:keys [name options] :as fieldinfo} graph forms value expanded-cur change! transact! tempid!]
  ;;select* has parameterized markup fn todo
  [select* graph forms options value (expanded-cur [name]) change! transact! tempid!])


(defn select-ref-component [{:keys [name] :as fieldinfo} graph forms value expanded-cur change! transact! tempid!]
  (form graph value (name forms) expanded-cur forms transact! tempid!))


(defn multi-select-ref [fieldinfo graph forms value expanded-cur change! transact! tempid!]
  (multi-select*
    multi-select-markup
    fieldinfo graph forms value expanded-cur change! #(change! [:db/add nil]) transact! tempid!)) ;add-item! is: add nil to set


(defn multi-select-ref-component [{:keys [options] :as fieldinfo} graph forms value expanded-cur change! transact! tempid!]
  [multi-select*
   multi-select-markup
   fieldinfo graph forms value expanded-cur change! #(change! [:db/add :temp-tempid]) transact! tempid!]) ;add new entity to set


(defn keyword-input [fieldinfo graph forms value expanded-cur change! transact! tempid!]
  [input* {:type "text"
           :value (str value)
           :on-change #(change! [:db/add (keyword (subs % 1))])}]) ;this seems broken


(defn code-editor [fieldinfo graph forms value expanded-cur change! transact! tempid!]
  ^{:key (:name fieldinfo)}
  [code-editor* value change!])


(defn valid-date-str? [s]
  (let [ms (.parse js/Date s)]                              ; NaN if not valid string
    (integer? ms)))


(defn instant [fieldinfo graph forms value expanded-cur change! transact! tempid!]
  (let [intermediate-val (reagent/atom (.toISOString value))]
    (fn [fieldinfo graph forms value expanded-cur change! transact! tempid!]
      [:input {:type "text"
               :class (if-not (valid-date-str? @intermediate-val) "invalid")
               :value @intermediate-val
               :on-change (fn [e]
                            (let [input-str (.. e -target -value)]
                              (reset! intermediate-val input-str) ;always save what they are typing
                              (if (valid-date-str? input-str)
                                (change! [:db/retract value]
                                         [:db/add (let [ms (.parse js/Date input-str)] (js/Date. ms))]))))}])))


(defn default [fieldinfo graph forms value expanded-cur change! transact! tempid!]
  [input* {:type "text"
           :value (str (select-keys fieldinfo [:datatype :cardinality :component]))
           :read-only true}])
