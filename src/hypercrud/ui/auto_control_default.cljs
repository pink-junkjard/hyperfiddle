(ns hypercrud.ui.auto-control-default
  (:require [hypercrud.ui.auto-control :as auto-control]
            [hypercrud.ui.form :as form]
            [hypercrud.ui.table :as table]
            [hypercrud.ui.table-cell :as table-cell]
            [hypercrud.ui.widget :as widget]))


(defmethod auto-control/auto-control :default
  [field anchors props param-ctx]
  ;(assert (:entity param-ctx))

  (let [isComponent (-> (:attribute param-ctx) :db/isComponent)
        valueType (-> (:attribute param-ctx) :db/valueType :db/ident)
        cardinality (-> (:attribute param-ctx) :db/cardinality :db/ident)
        widget (cond
                 (and (= valueType :db.type/boolean) (= cardinality :db.cardinality/one)) widget/boolean
                 (and (= valueType :db.type/keyword) (= cardinality :db.cardinality/one)) widget/keyword
                 (and (= valueType :db.type/string) (= cardinality :db.cardinality/one)) widget/string
                 ;(and (= valueType :db.type/long) (= cardinality :db.cardinality/one)) widget/long
                 (and (= valueType :db.type/instant) (= cardinality :db.cardinality/one)) widget/instant
                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/one) isComponent) widget/ref-component
                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/one)) widget/ref
                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/many) isComponent) widget/ref-many-table
                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/many)) widget/edn-many
                 :else widget/edn)]
    (widget field anchors props param-ctx)))


(defmethod auto-control/auto-table-cell :default
  [field anchors props param-ctx]
  (let [isComponent (-> (:attribute param-ctx) :db/isComponent)
        valueType (-> (:attribute param-ctx) :db/valueType :db/ident)
        cardinality (-> (:attribute param-ctx) :db/cardinality :db/ident)
        widget (cond
                 (and (= valueType :db.type/boolean) (= cardinality :db.cardinality/one)) widget/boolean
                 (and (= valueType :db.type/keyword) (= cardinality :db.cardinality/one)) widget/keyword
                 (and (= valueType :db.type/string) (= cardinality :db.cardinality/one)) widget/string
                 ;(and (= valueType :db.type/long) (= cardinality :db.cardinality/one)) widget/long
                 (and (= valueType :db.type/instant) (= cardinality :db.cardinality/one)) widget/instant
                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/one) isComponent) table-cell/ref-one-component
                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/one)) widget/ref
                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/many) isComponent) table-cell/ref-many
                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/many)) widget/edn-many
                 (and (= cardinality :db.cardinality/many)) widget/edn-many
                 (and (= cardinality :db.cardinality/one)) widget/edn
                 :else widget/edn)]
    (widget field anchors props param-ctx)))

(defmethod auto-control/result :default [result ordered-fes anchors param-ctx]
  [:div.auto-result
   (cond
     (map? result) [form/form result ordered-fes anchors param-ctx]
     (coll? result) [table/ui-table result ordered-fes anchors param-ctx]
     :else
     [:div.blank
      (widget/render-anchors (->> anchors
                                  (remove :anchor/find-element)
                                  (remove :anchor/attribute)
                                  (remove :anchor/render-inline?))
                             param-ctx)
      (widget/render-inline-anchors (->> anchors
                                         (remove :anchor/find-element)
                                         (remove :anchor/attribute)
                                         (filter :anchor/render-inline?))
                                    (dissoc param-ctx :isComponent))])])
