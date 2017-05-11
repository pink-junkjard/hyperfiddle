(ns hypercrud.ui.auto-control-default
  (:require [hypercrud.ui.auto-control :as auto-control]
            [hypercrud.ui.form :as form]
            [hypercrud.ui.table :as table]
            [hypercrud.ui.table-cell :as table-cell]
            [hypercrud.ui.widget :as widget]))

(defmethod auto-control/auto-control :default
  [maybe-field anchors props param-ctx]
  ;(assert (:entity param-ctx))

  (let [isComponent (-> (:attribute param-ctx) :attribute/isComponent)
        valueType (-> (:attribute param-ctx) :attribute/valueType :db/ident)
        cardinality (-> (:attribute param-ctx) :attribute/cardinality :db/ident)
        widget (cond
                 (and (= valueType :db.type/boolean) (= cardinality :db.cardinality/one)) widget/boolean
                 (and (= valueType :db.type/keyword) (= cardinality :db.cardinality/one)) widget/keyword
                 (and (= valueType :db.type/string) (= cardinality :db.cardinality/one)) widget/string
                 (and (= valueType :db.type/long) (= cardinality :db.cardinality/one)) widget/long
                 (and (= valueType :db.type/instant) (= cardinality :db.cardinality/one)) widget/instant
                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/one) isComponent) widget/ref-component
                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/one)) widget/ref
                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/many) isComponent) widget/ref-many-table
                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/many)) widget/ref-many
                 :else widget/raw)]
    (widget maybe-field anchors props param-ctx)))


(defmethod auto-control/auto-table-cell :default
  [maybe-field anchors props param-ctx]
  (let [isComponent (-> (:attribute param-ctx) :attribute/isComponent)
        valueType (-> (:attribute param-ctx) :attribute/valueType :db/ident)
        cardinality (-> (:attribute param-ctx) :attribute/cardinality :db/ident)
        widget (cond
                 (and (= valueType :db.type/boolean) (= cardinality :db.cardinality/one)) widget/boolean
                 (and (= valueType :db.type/keyword) (= cardinality :db.cardinality/one)) widget/keyword
                 (and (= valueType :db.type/string) (= cardinality :db.cardinality/one)) widget/string
                 (and (= valueType :db.type/long) (= cardinality :db.cardinality/one)) widget/long
                 (and (= valueType :db.type/instant) (= cardinality :db.cardinality/one)) widget/instant
                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/one) isComponent) table-cell/ref-one-component
                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/one)) widget/ref
                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/many) isComponent) table-cell/ref-many
                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/many)) widget/ref-many
                 (and (= cardinality :db.cardinality/many)) table-cell/other-many
                 :else widget/raw)]
    (widget maybe-field anchors props param-ctx)))

(defmethod auto-control/result :default [result colspec anchors param-ctx]
  (cond
    (map? result) (form/form result colspec anchors param-ctx)
    (coll? result) [table/table result colspec anchors param-ctx]
    :else nil)

  #_[:div
   ; This has to be handled internally due to weirdness around create-new links
   ; where we need to conjure a connection. It would be more elegant if we conjured the
   ; connection up here, which is possible by inspecting the result in entity-syslink case.
   #_(widget/render-anchors (->> anchors
                                 (remove :anchor/repeating?)
                                 (remove :anchor/attribute)
                                 (remove :anchor/render-inline?))
                            (dissoc param-ctx :isComponent))
   (cond
     ; order matters here
     (map? result) (form/form result colspec anchors param-ctx)
     (coll? result) [table/table result colspec anchors param-ctx]         ; stateful
     :else nil)])
