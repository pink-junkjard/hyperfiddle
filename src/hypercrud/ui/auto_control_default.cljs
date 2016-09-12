(ns hypercrud.ui.auto-control-default
  (:require [hypercrud.ui.auto-control :as auto-control]
            [hypercrud.ui.widget :as widget]))


(defmethod auto-control/auto-control :default
  [value {:keys [change!] {:keys [:attribute/valueType :attribute/cardinality :attribute/isComponent] :as field} :field :as widget-args}]
  (cond
    (and (= valueType :keyword) (= cardinality :db.cardinality/one)) (widget/input-keyword value change!)
    (and (= valueType :string) (= cardinality :db.cardinality/one)) (widget/input value change!)
    (and (= valueType :code) (= cardinality :db.cardinality/one)) (widget/code-editor field value change!)
    (and (= valueType :instant) (= cardinality :db.cardinality/one)) (widget/instant value change!)
    (and (= valueType :ref) (= cardinality :db.cardinality/one) isComponent) (widget/select-ref-component value widget-args)
    (and (= valueType :ref) (= cardinality :db.cardinality/many) isComponent) (widget/multi-select-ref-component value widget-args)
    (and (= valueType :ref) (= cardinality :db.cardinality/one)) (widget/select-ref value widget-args)
    (and (= valueType :ref) (= cardinality :db.cardinality/many)) (widget/multi-select-ref field widget-args)
    :else (widget/default field)))
