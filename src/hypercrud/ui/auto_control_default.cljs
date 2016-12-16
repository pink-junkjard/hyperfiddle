(ns hypercrud.ui.auto-control-default
  (:require [hypercrud.ui.auto-control :as auto-control]
            [hypercrud.ui.table-cell :as table-cell]
            [hypercrud.ui.select :as select]
            [hypercrud.ui.widget :as widget]))


; Auto-control takes the parent entity as context
; We think this is only used for the dbid, so we can create a tx
; If the parent was someday needed for dispatching, there are better things to dispatch on,
; for example the entire graph can be available in dynamic scope for specific queries, no
; need to limit it to parent.

(defmethod auto-control/auto-control :default
  [entity field links param-ctx]
  (let [{:keys [:attribute/valueType :attribute/cardinality :attribute/isComponent]} (:field/attribute field)
        valueType (:db/ident valueType)
        cardinality (:db/ident cardinality)]
    (cond
      (and (= valueType :db.type/boolean) (= cardinality :db.cardinality/one)) (select/select-boolean entity field links param-ctx)
      (and (= valueType :db.type/keyword) (= cardinality :db.cardinality/one)) (widget/input-keyword entity field links param-ctx)
      (and (= valueType :db.type/string) (= cardinality :db.cardinality/one)) (widget/input entity field links param-ctx)
      (and (= valueType :db.type/long) (= cardinality :db.cardinality/one)) (widget/input-long entity field links param-ctx)
      (and (= valueType :db.type/code) (= cardinality :db.cardinality/one)) (widget/code-editor entity field links param-ctx)
      (and (= valueType :db.type/instant) (= cardinality :db.cardinality/one)) (widget/instant entity field links param-ctx)
      (and (= valueType :db.type/ref) (= cardinality :db.cardinality/one) isComponent) (widget/select-ref-component entity field links param-ctx)
      (and (= valueType :db.type/ref) (= cardinality :db.cardinality/many) isComponent) (widget/table-many-ref-component entity field links param-ctx)
      (and (= valueType :db.type/ref) (= cardinality :db.cardinality/one)) (widget/select-ref entity field links param-ctx)
      (and (= valueType :db.type/ref) (= cardinality :db.cardinality/many)) [widget/table-many-ref entity field links param-ctx]
      :else (widget/default field))))


(defmethod auto-control/auto-table-cell :default
  [entity field links param-ctx]
  (let [{:keys [:attribute/valueType :attribute/cardinality :attribute/isComponent]} (:field/attribute field)
        valueType (:db/ident valueType)
        cardinality (:db/ident cardinality)]
    (cond
      (and (= valueType :db.type/boolean) (= cardinality :db.cardinality/one)) (select/select-boolean entity field links param-ctx)
      (and (= valueType :db.type/keyword) (= cardinality :db.cardinality/one)) (widget/input-keyword entity field links param-ctx)
      (and (= valueType :db.type/string) (= cardinality :db.cardinality/one)) (widget/input entity field links param-ctx)
      (and (= valueType :db.type/long) (= cardinality :db.cardinality/one)) (widget/input-long entity field links param-ctx)
      (and (= valueType :db.type/code) (= cardinality :db.cardinality/one)) (widget/input entity field links param-ctx)
      (and (= valueType :db.type/instant) (= cardinality :db.cardinality/one)) (widget/instant entity field links param-ctx)

      (and (= valueType :db.type/ref) (= cardinality :db.cardinality/one) isComponent) (table-cell/ref-one-component entity field links param-ctx)
      (and (= valueType :db.type/ref) (= cardinality :db.cardinality/one)) (widget/select-ref entity field links param-ctx)

      (and (= valueType :db.type/ref) (= cardinality :db.cardinality/many)) (table-cell/ref-many entity field links param-ctx)
      (and (= cardinality :db.cardinality/many)) (table-cell/other-many entity field links param-ctx)

      :else (widget/default field))))
