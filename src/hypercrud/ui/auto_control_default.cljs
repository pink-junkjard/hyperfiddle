(ns hypercrud.ui.auto-control-default
  (:require [cljs.reader :as reader]
            [hypercrud.browser.links :as links]
            [hypercrud.types :as types]
            [hypercrud.ui.auto-control :as auto-control]
            [hypercrud.ui.form :as form]
            [hypercrud.ui.table :as table]
            [hypercrud.ui.table-cell :as table-cell]
            [hypercrud.ui.widget :as widget]
            [hypercrud.util :as util]
            [hypercrud.ui.input :as input]
            [hypercrud.client.tx :as tx]
            [hypercrud.browser.system-links :as system-links]
            [hypercrud.ui.form-util :as form-util]))


(defn build-props [value maybe-field anchors param-ctx]
  ; why does this need the field - it needs the ident for readonly in "Edit Anchors"
  {:read-only ((get param-ctx :read-only) (:attribute param-ctx) param-ctx)})


(defmethod auto-control/auto-control :default
  [value maybe-field anchors param-ctx]
  (let [isComponent (-> (:attribute param-ctx) :attribute/isComponent)
        valueType (-> (:attribute param-ctx) :attribute/valueType :db/ident)
        cardinality (-> (:attribute param-ctx) :attribute/cardinality :db/ident)
        props (build-props value maybe-field anchors param-ctx)
        widget (cond
                 (and (= valueType :db.type/boolean) (= cardinality :db.cardinality/one)) widget/boolean
                 (and (= valueType :db.type/keyword) (= cardinality :db.cardinality/one)) widget/keyword
                 (and (= valueType :db.type/string) (= cardinality :db.cardinality/one)) widget/string
                 (and (= valueType :db.type/long) (= cardinality :db.cardinality/one)) widget/long
                 (and (= valueType :db.type/code) (= cardinality :db.cardinality/one)) widget/code
                 (and (= valueType :db.type/instant) (= cardinality :db.cardinality/one)) widget/instant
                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/one) isComponent) widget/ref-component
                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/many) isComponent) widget/ref-many-component-table
                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/one)) widget/ref
                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/many)) widget/ref-many-table
                 :else widget/raw
                 )]
    (widget value maybe-field anchors props param-ctx)))


(defmethod auto-control/auto-table-cell :default
  [value maybe-field anchors param-ctx]
  (let [isComponent (-> (:attribute param-ctx) :attribute/isComponent)
        valueType (-> (:attribute param-ctx) :attribute/valueType :db/ident)
        cardinality (-> (:attribute param-ctx) :attribute/cardinality :db/ident)
        props (build-props value maybe-field anchors param-ctx)
        widget (cond
                 (and (= valueType :db.type/boolean) (= cardinality :db.cardinality/one)) widget/boolean
                 (and (= valueType :db.type/keyword) (= cardinality :db.cardinality/one)) widget/keyword
                 (and (= valueType :db.type/string) (= cardinality :db.cardinality/one)) widget/string
                 (and (= valueType :db.type/long) (= cardinality :db.cardinality/one)) widget/long
                 (and (= valueType :db.type/code) (= cardinality :db.cardinality/one)) widget/string
                 (and (= valueType :db.type/instant) (= cardinality :db.cardinality/one)) widget/instant
                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/one) isComponent) table-cell/ref-one-component
                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/one)) widget/ref
                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/many)) table-cell/ref-many
                 (and (= cardinality :db.cardinality/many)) table-cell/other-many
                 :else widget/raw
                 )]
    (widget value maybe-field anchors props param-ctx)))





(defn no-link-type [anchors param-ctx]
  (let [non-repeating-top-anchors (->> anchors
                                       (remove :anchor/repeating?)
                                       (remove :anchor/find-element)
                                       (remove :anchor/attribute))]
    [:div
     (widget/render-anchors (remove :anchor/render-inline? non-repeating-top-anchors) param-ctx)
     (let [param-ctx (dissoc param-ctx :isComponent)]
       (widget/render-inline-links (filter :anchor/render-inline? non-repeating-top-anchors) param-ctx))]))


(defn form-or-sys-form [resultset colspec link param-ctx]
  (let [c (if (system-links/system-link? (:db/id link))
            form/sys-form
            form/form)]
    (c resultset colspec link param-ctx)))


(defmethod auto-control/resultset :default [type resultset colspec anchors param-ctx]
  (case type
    :link-query [table/table resultset colspec anchors param-ctx] ; stateful
    :link-entity (form-or-sys-form resultset colspec anchors param-ctx)
    (no-link-type anchors param-ctx)))
