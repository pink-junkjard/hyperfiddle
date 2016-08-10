(ns hypercrud.ui.widget
  (:require [hypercrud.ui.auto-control :refer [auto-control]]
            [hypercrud.ui.code-editor :refer [code-editor*]]
            [hypercrud.ui.form :refer [form]]
            [hypercrud.ui.input :refer [input*]]
            [hypercrud.ui.multi-select :refer [multi-select* multi-select-markup]]
            [hypercrud.ui.select :refer [select*]]
            [hypercrud.ui.textarea :refer [textarea*]]))


(defn input [fieldinfo graph forms value expanded-cur change! transact! tempid!]
  [input* {:type "text"
           :value value
           :on-change change!}])


(defn textarea [fieldinfo graph forms value expanded-cur change! transact! tempid!]
  [textarea* {:type "text"
              :value value
              :on-change change!}])


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
           :on-change #(change! [:db/add (keyword (subs % 1))])}])


(defn code-editor [fieldinfo graph forms value expanded-cur change! transact! tempid!]
  ^{:key (:name fieldinfo)}
  [code-editor* value change!])


(defn default [fieldinfo graph forms value expanded-cur change! transact! tempid!]
  [input* {:type "text"
           :value (str (select-keys fieldinfo [:datatype :cardinality :component]))
           :read-only true}])
