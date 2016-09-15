(ns hypercrud.ui.auto-control-default
  (:require [hypercrud.ui.auto-control :as auto-control]
            [hypercrud.ui.table-cell :as table-cell]
            [hypercrud.ui.widget :as widget]))


(defmethod auto-control/auto-control :default
  [entity {{:keys [:valueType :cardinality :isComponent] :as field} :field :as widget-args}]
  (cond
    ;(and (= valueType :boolean) (= cardinality :db.cardinality/one)) (widget/select-ref entity widget-args)
    (and (= valueType :keyword) (= cardinality :db.cardinality/one)) (widget/input-keyword entity widget-args)
    (and (= valueType :string) (= cardinality :db.cardinality/one)) (widget/input entity widget-args)
    (and (= valueType :code) (= cardinality :db.cardinality/one)) (widget/code-editor entity widget-args)
    (and (= valueType :instant) (= cardinality :db.cardinality/one)) (widget/instant entity widget-args)
    (and (= valueType :ref) (= cardinality :db.cardinality/one) isComponent) (widget/select-ref-component entity widget-args)
    (and (= valueType :ref) (= cardinality :db.cardinality/many) isComponent) (widget/table-many-ref-component entity widget-args)
    (and (= valueType :ref) (= cardinality :db.cardinality/one)) (widget/select-ref entity widget-args)
    (and (= valueType :ref) (= cardinality :db.cardinality/many)) (widget/table-many-ref entity widget-args)
    :else (widget/default field)))


(defmethod auto-control/auto-table-cell :default
  [entity form-id {{:keys [:valueType :cardinality :isComponent] :as field} :field :as widget-args}]
  (cond
    (and (= valueType :keyword) (= cardinality :db.cardinality/one)) (widget/input-keyword entity widget-args)
    (and (= valueType :string) (= cardinality :db.cardinality/one)) (widget/input entity widget-args)
    ;(and (= valueType :code) (= cardinality :db.cardinality/one)) (widget/code-editor entity widget-args) todo
    (and (= valueType :instant) (= cardinality :db.cardinality/one)) (widget/instant entity widget-args)

    (and (= valueType :ref) (= cardinality :db.cardinality/one) isComponent) (table-cell/ref-one-component entity form-id widget-args)
    (and (= valueType :ref) (= cardinality :db.cardinality/one)) (table-cell/ref-one entity form-id widget-args)

    (and (= valueType :ref) (= cardinality :db.cardinality/many)) (table-cell/ref-many entity form-id widget-args)
    (and (= cardinality :db.cardinality/many)) (table-cell/other-many entity form-id widget-args)

    :else (widget/default field)))
