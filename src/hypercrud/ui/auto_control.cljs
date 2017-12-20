(ns hypercrud.ui.auto-control
  (:require [cuerdas.core :as str]
            [hypercrud.browser.auto-anchor :as auto-anchor]
            [hypercrud.ui.connection-color :as connection-color]
            [hypercrud.ui.control.markdown-rendered :refer [markdown-rendered*]]
            [hypercrud.ui.css :refer [css-slugify]]
            [hypercrud.ui.attribute.instant :as instant]
            [hypercrud.ui.attribute.edn :as edn]
            [hypercrud.ui.table-cell :as table-cell]
            [hypercrud.ui.user-attribute-renderer :as renderer]
            [hypercrud.ui.widget :as widget]
            [hypercrud.util.reactive :as reactive]))


(defn schema-control-form [field links props ctx]
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
    (widget field links props ctx)))

; Can be unified; inspect (:layout ctx)
(defn schema-control-table [field links props ctx]
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
    (widget field links props ctx)))

; What even is this scar
(defn control-props [field links ctx]
  ; why does this need the field - it needs the ident for readonly in "Edit Anchors"
  ; todo clean this interface up
  {:read-only ((get ctx :read-only) (:attribute ctx) ctx)})

(defn auto-control [field links ctx]
  (let [; control override at schema level (Not really an override yet, but soon)
        control (case (:layout ctx) :block schema-control-form
                                    :inline-block schema-control-table
                                    :table schema-control-table)

        ; control override at attribute level
        control (if (renderer/user-attribute-renderer ctx)
                  (renderer/user-attribute-render field links ctx)
                  control)

        ; control override at fiddle/ctx level
        control (case @(:display-mode ctx) :xray control :user (get ctx :control control))]

    ; Old comment, what does this mean now: (I think it means nothing, field is dead)
    ; --What is the user-field allowed to change? The ctx. Can it change links or anchors? no.
    ;
    ; todo control can have access to repeating contextual values (color, owner, result, entity, value, etc) but field should NOT
    ; this leads to inconsistent location formulas between non-repeating links in tables vs forms

    ; Return value just needs a ctx.
    ; Dynamic logic is done; user can't further override it with the field-ctx
    (reactive/partial control field links (control-props field links ctx))))
