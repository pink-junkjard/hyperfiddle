(ns hypercrud.ui.auto-control
  (:require [hypercrud.ui.table-cell :as table-cell]
            [hypercrud.ui.attribute.instant :as instant]
            [hypercrud.ui.attribute.edn :as edn]
            [hypercrud.ui.widget :as widget]))


(defn auto-control [field anchors props ctx]
  (let [isComponent (-> (:attribute ctx) :db/isComponent)
        valueType (-> (:attribute ctx) :db/valueType :db/ident)
        cardinality (-> (:attribute ctx) :db/cardinality :db/ident)
        widget (cond
                 (and (= valueType :db.type/boolean) (= cardinality :db.cardinality/one)) widget/boolean
                 (and (= valueType :db.type/keyword) (= cardinality :db.cardinality/one)) widget/keyword
                 (and (= valueType :db.type/string) (= cardinality :db.cardinality/one)) widget/string
                 ;(and (= valueType :db.type/long) (= cardinality :db.cardinality/one)) widget/long
                 (and (= valueType :db.type/instant) (= cardinality :db.cardinality/one)) instant/instant
                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/one) isComponent) widget/ref-component
                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/one)) widget/ref
                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/many) isComponent) widget/ref-many-table
                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/many)) edn/edn-many
                 :else edn/edn)]
    (widget field anchors props ctx)))

(defn auto-table-cell [field anchors props ctx]
  (let [isComponent (-> (:attribute ctx) :db/isComponent)
        valueType (-> (:attribute ctx) :db/valueType :db/ident)
        cardinality (-> (:attribute ctx) :db/cardinality :db/ident)
        widget (cond
                 (and (= valueType :db.type/boolean) (= cardinality :db.cardinality/one)) widget/boolean
                 (and (= valueType :db.type/keyword) (= cardinality :db.cardinality/one)) widget/keyword
                 (and (= valueType :db.type/string) (= cardinality :db.cardinality/one)) widget/string
                 ;(and (= valueType :db.type/long) (= cardinality :db.cardinality/one)) widget/long
                 (and (= valueType :db.type/instant) (= cardinality :db.cardinality/one)) instant/instant
                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/one) isComponent) table-cell/ref-one-component
                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/one)) widget/ref
                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/many) isComponent) table-cell/ref-many
                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/many)) edn/edn-many
                 (and (= cardinality :db.cardinality/many)) widget/edn-many
                 (and (= cardinality :db.cardinality/one)) widget/edn
                 :else edn/edn)]
    (widget field anchors props ctx)))
