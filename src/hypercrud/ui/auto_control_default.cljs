(ns hypercrud.ui.auto-control-default
  (:require [hypercrud.ui.auto-control :as auto-control]
            [hypercrud.ui.widget :as widget]))


(defn widget-for-fieldinfo [{:keys [:attribute/valueType :attribute/cardinality :attribute/isComponent]}]
  (cond
    (and (= valueType :keyword) (= cardinality :db.cardinality/one)) widget/input-keyword
    (and (= valueType :string) (= cardinality :db.cardinality/one)) widget/input
    (and (= valueType :code) (= cardinality :db.cardinality/one)) widget/code-editor
    (and (= valueType :instant) (= cardinality :db.cardinality/one)) widget/instant
    (and (= valueType :ref) (= cardinality :db.cardinality/one) isComponent) widget/select-ref-component
    (and (= valueType :ref) (= cardinality :db.cardinality/many) isComponent) widget/multi-select-ref-component
    (and (= valueType :ref) (= cardinality :db.cardinality/one)) widget/select-ref
    (and (= valueType :ref) (= cardinality :db.cardinality/many)) widget/multi-select-ref
    :else widget/default))


(defmethod auto-control/auto-control :default
  [fieldinfo graph forms value expanded-cur change! transact! tempid!]
  [(widget-for-fieldinfo fieldinfo) fieldinfo graph forms value expanded-cur change! transact! tempid!])
