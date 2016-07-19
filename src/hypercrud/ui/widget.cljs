(ns hypercrud.ui.widget
  (:require [hypercrud.ui.auto-control :refer [auto-control]]
            [hypercrud.ui.form :refer [cj-form]]
            [hypercrud.ui.input :refer [input*]]
            [hypercrud.ui.multi-select :refer [multi-select* multi-select-markup]]
            [hypercrud.ui.select :refer [select*]]))


(defn input [fieldinfo graph forms value change! transact!]
  [input* {:type "text"
           :value value
           :on-change change!}])


(defn select-ref [{:keys [options] :as fieldinfo} graph forms value change! transact!]
  ;;select* has parameterized markup fn todo
  [select* graph forms options value change! transact!])


(defn select-ref-component [fieldinfo graph forms value change! transact!]
  (cj-form graph value forms transact!))


(defn multi-select-ref [fieldinfo graph forms value change! transact!]
  (multi-select*
    multi-select-markup
    fieldinfo graph forms value change! #(change! [:db/add nil]) transact!)) ;add-item! is: add nil to set


(defn multi-select-ref-component [{:keys [options] :as fieldinfo} graph forms value change! transact!]
  [multi-select*
   multi-select-markup
   fieldinfo graph forms value change! #(change! [:db/add :temp-tempid]) transact!]) ;add new entity to set


(defn keyword-input [fieldinfo graph forms value change! transact!]
  [input* {:type "text"
           :value (str value)
           :on-change #(change! [:db/add (keyword (subs % 1))])}])


(defn default [fieldinfo graph forms value change! transact!]
  [input* {:type "text"
           :value (str (select-keys fieldinfo [:datatype :set :component]))
           :read-only true}])
