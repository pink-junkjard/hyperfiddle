(ns hypercrud.ui.auto-control
  (:require
    [contrib.reactive :as r]
    [hypercrud.ui.attribute.edn :as edn]
    [hypercrud.ui.attribute.instant :as instant]
    [hypercrud.ui.safe-render :refer [portal-markup user-portal]]
    [hypercrud.ui.table-cell :as table-cell]
    [hypercrud.ui.util :refer [attr-renderer]]
    [hypercrud.ui.widget :as widget]))


(defn schema-control-form [ctx]
  (let [isComponent @(r/cursor (:hypercrud.browser/fat-attribute ctx) [:db/isComponent])
        valueType @(r/cursor (:hypercrud.browser/fat-attribute ctx) [:db/valueType :db/ident])
        cardinality @(r/cursor (:hypercrud.browser/fat-attribute ctx) [:db/cardinality :db/ident])
        widget (cond
                 (and (= valueType :db.type/boolean) (= cardinality :db.cardinality/one)) widget/boolean
                 (and (= valueType :db.type/keyword) (= cardinality :db.cardinality/one)) widget/keyword
                 (and (= valueType :db.type/string) (= cardinality :db.cardinality/one)) widget/string
                 (and (= valueType :db.type/long) (= cardinality :db.cardinality/one)) widget/long
                 (and (= valueType :db.type/instant) (= cardinality :db.cardinality/one)) instant/instant
                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/one) isComponent) widget/ref-component
                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/one)) widget/ref
                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/many) isComponent) widget/ref-many-table
                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/many)) edn/edn-many
                 (nil? valueType) widget/text
                 :else edn/edn)]
    widget))

; Can be unified; inspect (:layout ctx)
(defn schema-control-table [ctx]
  (let [isComponent @(r/cursor (:hypercrud.browser/fat-attribute ctx) [:db/isComponent])
        valueType @(r/cursor (:hypercrud.browser/fat-attribute ctx) [:db/valueType :db/ident])
        cardinality @(r/cursor (:hypercrud.browser/fat-attribute ctx) [:db/cardinality :db/ident])
        widget (cond
                 (and (= valueType :db.type/boolean) (= cardinality :db.cardinality/one)) widget/boolean
                 (and (= valueType :db.type/keyword) (= cardinality :db.cardinality/one)) widget/keyword
                 (and (= valueType :db.type/string) (= cardinality :db.cardinality/one)) widget/string
                 (and (= valueType :db.type/long) (= cardinality :db.cardinality/one)) widget/long
                 (and (= valueType :db.type/instant) (= cardinality :db.cardinality/one)) instant/instant
                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/one) isComponent) table-cell/ref-one-component
                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/one)) widget/ref
                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/many) isComponent) table-cell/ref-many
                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/many)) edn/edn-many
                 (and (= cardinality :db.cardinality/many)) edn/edn-many
                 (and (= cardinality :db.cardinality/one)) edn/edn
                 (nil? valueType) widget/text
                 :else edn/edn)]
    widget))

(defn auto-control [ctx]
  ; todo binding renderers should be pathed for aggregates and values
  ;
  ; Old comment, what does this mean now: (I think it means nothing, field is dead)
  ; --What is the user-field allowed to change? The ctx. Can it change links or anchors? no.
  ;
  ; todo control can have access to repeating contextual values (color, result, entity, value, etc) but field should NOT
  ; this leads to inconsistent location formulas between non-repeating links in tables vs forms
  ;
  ; Return value just needs a ctx.
  ; Dynamic logic is done; user can't further override it with the field-ctx

  ; This is not quite right; each stage wants to be able to wrap the stage before.
  ; So it's kind of backwards right now and user-controls have
  ; knowledge of this pipeline.

  (let [attribute @(:hypercrud.browser/fat-attribute ctx)]
    (or (case @(:hypercrud.ui/display-mode ctx)
          :hypercrud.browser.browser-ui/user (some->> (:control ctx) (r/partial portal-markup))
          :hypercrud.browser.browser-ui/xray nil)
        (attr-renderer (:attribute/renderer attribute) ctx)
        (some->> (case (:hyperfiddle.ui/layout ctx :hyperfiddle.ui.layout/block)
                   :hyperfiddle.ui.layout/block (schema-control-form ctx)
                   :hyperfiddle.ui.layout/inline-block (schema-control-table ctx)
                   :hyperfiddle.ui.layout/table (schema-control-table ctx))
                 (r/partial portal-markup)))))

(defn control-props [ctx]
  ; Only used by fiddle-links/bindings which are legacy, we do that stuff in a renderer now.
  (cond-> {} (:read-only ctx) (assoc :read-only ((:read-only ctx) ctx))))
