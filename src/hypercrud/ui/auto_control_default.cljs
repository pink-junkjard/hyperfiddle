(ns hypercrud.ui.auto-control-default
  (:require [cljs.core.match :refer-macros [match]]
            [hypercrud.ui.auto-control :as auto-control]
            [hypercrud.ui.widget :as widget]))


(defn widget-for-fieldinfo [fieldinfo]
  (match [fieldinfo]
         [{:datatype :string :set false}] widget/input

         [{:datatype :ref :set false :component true}] widget/select-ref-component
         [{:datatype :ref :set true :component true}] widget/multi-select-ref-component
         [{:datatype :ref :set false}] widget/select-ref
         [{:datatype :ref :set true}] widget/multi-select-ref
         :else widget/default))


(defmethod auto-control/auto-control :default [fieldinfo graph forms value change! transact! tempid!]
  [(widget-for-fieldinfo fieldinfo) fieldinfo graph forms value change! transact! tempid!])
