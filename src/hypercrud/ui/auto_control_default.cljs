(ns hypercrud.ui.auto-control-default
  (:require [hypercrud.ui.auto-control :as auto-control]
            [hypercrud.ui.widget :as widget]))


(defn widget-for-fieldinfo [{:keys [datatype cardinality component]}]
  (cond
    (and (= datatype :string) (= cardinality :one)) widget/input
    (and (= datatype :code) (= cardinality :one)) widget/code-editor
    (and (= datatype :instant) (= cardinality :one)) widget/instant
    (and (= datatype :ref) (= cardinality :one) component) widget/select-ref-component
    (and (= datatype :ref) (= cardinality :many) component) widget/multi-select-ref-component
    (and (= datatype :ref) (= cardinality :one)) widget/select-ref
    (and (= datatype :ref) (= cardinality :many)) widget/multi-select-ref
    :else widget/default))


(defmethod auto-control/auto-control :default
  [fieldinfo graph forms value expanded-cur change! transact! tempid!]
  [(widget-for-fieldinfo fieldinfo) fieldinfo graph forms value expanded-cur change! transact! tempid!])
