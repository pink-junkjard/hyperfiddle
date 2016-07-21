(ns hypercrud.ui.auto-control-default
  (:require [cljs.core.match :refer-macros [match]]
            [hypercrud.ui.auto-control :as auto-control]
            [hypercrud.ui.widget :as widget]))


(defn widget-for-fieldinfo [fieldinfo]
  (match [fieldinfo]
         [{:datatype :string :cardinality :one}] widget/input

         [{:datatype :ref :cardinality :one :component true}] widget/select-ref-component
         [{:datatype :ref :cardinality :many :component true}] widget/multi-select-ref-component
         [{:datatype :ref :cardinality :one}] widget/select-ref
         [{:datatype :ref :cardinality :many}] widget/multi-select-ref
         :else widget/default))


(defmethod auto-control/auto-control :default
  [fieldinfo graph metatype forms value expanded-cur change! transact! tempid!]
  [(widget-for-fieldinfo fieldinfo) fieldinfo graph metatype forms value expanded-cur change! transact! tempid!])
