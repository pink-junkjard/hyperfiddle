(ns hypercrud.ui.widget
  (:require [hypercrud.ui.auto-control :refer [auto-control]]
            [hypercrud.ui.form :refer [form]]
            [hypercrud.ui.input :refer [input*]]
            [hypercrud.ui.multi-select :refer [multi-select* multi-select-markup]]
            [hypercrud.ui.select :refer [select*]]
            [hypercrud.ui.textarea :refer [textarea*]]))


(defn input [fieldinfo graph metatype forms value expanded-cur change! transact! tempid!]
  [input* {:type "text"
           :value value
           :on-change change!}])


(defn textarea [fieldinfo graph metatype forms value expanded-cur change! transact! tempid!]
  [textarea* {:type "text"
              :value value
              :on-change change!}])


(defn select-ref [{:keys [name options] :as fieldinfo} graph metatype forms value expanded-cur change! transact! tempid!]
  ;;select* has parameterized markup fn todo
  [select* graph forms options value (expanded-cur [name]) change! transact! tempid!])


(defn select-ref-component [fieldinfo graph metatype forms value expanded-cur change! transact! tempid!]
  (form graph value metatype expanded-cur forms transact! tempid!))


(defn multi-select-ref [fieldinfo graph metatype forms value expanded-cur change! transact! tempid!]
  (multi-select*
    multi-select-markup
    fieldinfo graph metatype forms value expanded-cur change! #(change! [:db/add nil]) transact! tempid!)) ;add-item! is: add nil to set


(defn multi-select-ref-component [{:keys [options] :as fieldinfo} graph metatype forms value expanded-cur change! transact! tempid!]
  [multi-select*
   multi-select-markup
   fieldinfo graph metatype forms value expanded-cur change! #(change! [:db/add :temp-tempid]) transact! tempid!]) ;add new entity to set


(defn keyword-input [fieldinfo graph metatype forms value expanded-cur change! transact! tempid!]
  [input* {:type "text"
           :value (str value)
           :on-change #(change! [:db/add (keyword (subs % 1))])}])


(defn default [fieldinfo graph metatype forms value expanded-cur change! transact! tempid!]
  [input* {:type "text"
           :value (str (select-keys fieldinfo [:datatype :cardinality :component]))
           :read-only true}])
