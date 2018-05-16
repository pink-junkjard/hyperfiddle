(ns hypercrud.ui.auto-control
  (:require [cats.monad.either :as either]
            [contrib.reactive :as r]
            [cuerdas.core :as string]
            [hypercrud.browser.browser-ui :as browser-ui]
            [hypercrud.ui.attribute.edn :as edn]
            [hypercrud.ui.attribute.instant :as instant]
            [hypercrud.ui.error :as ui-error]
            [hypercrud.ui.safe-render :refer [portal-markup user-portal]]
            [hypercrud.ui.table-cell :as table-cell]
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

(defn- ^:private safe-reagent-f [with-error f & args]
  ^{:key (hash f)}
  [user-portal with-error
   (into [f] args)])

(defn ^:deprecated fiddle-field-control [ctx]               ; use cell api
  (some->> (get-in ctx [:fields (:hypercrud.browser/attribute ctx) :renderer])
           (r/partial safe-reagent-f (ui-error/error-comp ctx))))

(defn attribute-control [ctx]
  (let [renderer @(r/cursor (:hypercrud.browser/fat-attribute ctx) [:attribute/renderer])]
    (when (and (string? renderer) (not (string/blank? renderer)))
      (let [with-error (ui-error/error-comp ctx)
            f browser-ui/eval-renderer-comp]
        (r/partial safe-reagent-f with-error f renderer)))))

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

  (or (case @(:hypercrud.ui/display-mode ctx) :user (some->> (:control ctx) (r/partial portal-markup)) :xray nil)
      (attribute-control ctx)
      (some->> (case (:layout ctx :block)
                 :block (schema-control-form ctx)
                 :inline-block (schema-control-table ctx)
                 :table (schema-control-table ctx))
               (r/partial portal-markup))))

(defn ^:deprecated control-props [ctx]
  ; Only used by fiddle-links/bindings which are legacy, we do that stuff in a renderer now.
  (cond-> {} (:read-only ctx) (assoc :read-only ((:read-only ctx) ctx))))

(comment
  ; Find a home for this:
  (def validators {"clojure" #(-> (safe-read-edn-string %) (either/right?))})

  (let [valid? ((get validators (:mode props) (constantly true)))
        class (string/join " " (list (if (:readOnly props) "read-only")
                                     (if (not valid?) "invalid")))])

  )
