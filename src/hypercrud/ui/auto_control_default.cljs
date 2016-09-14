(ns hypercrud.ui.auto-control-default
  (:require [hypercrud.ui.auto-control :as auto-control]
            [hypercrud.ui.widget :as widget]))


(defmethod auto-control/auto-control :default
  [entity {{:keys [:valueType :cardinality isComponent] :as field} :field :as widget-args}]
  (cond
    ;(and (= valueType :boolean) (= cardinality :db.cardinality/one)) (widget/select-ref entity widget-args)
    (and (= valueType :keyword) (= cardinality :db.cardinality/one)) (widget/input-keyword entity widget-args)
    (and (= valueType :string) (= cardinality :db.cardinality/one)) (widget/input entity widget-args)
    (and (= valueType :code) (= cardinality :db.cardinality/one)) (widget/code-editor entity widget-args)
    (and (= valueType :instant) (= cardinality :db.cardinality/one)) (widget/instant entity widget-args)
    (and (= valueType :ref) (= cardinality :db.cardinality/one) isComponent) (widget/select-ref-component entity widget-args)
    (and (= valueType :ref) (= cardinality :db.cardinality/many) isComponent) (widget/multi-select-ref-component entity widget-args)
    (and (= valueType :ref) (= cardinality :db.cardinality/one)) (widget/select-ref entity widget-args)
    (and (= valueType :ref) (= cardinality :db.cardinality/many)) (widget/multi-select-ref entity widget-args)
    :else (widget/default field)))
