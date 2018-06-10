(ns hypercrud.ui.auto-control
  (:require
    [contrib.reactive :as r]
    [contrib.string :refer [blank->nil]]
    [clojure.core.match :refer [match*]]
    [hypercrud.ui.attribute.edn :refer [edn edn-many]]
    [hypercrud.ui.attribute.instant :refer [instant]]
    [hypercrud.ui.safe-render :refer [portal-markup]]
    [hypercrud.ui.table-cell :as table-cell]
    [hypercrud.ui.util :refer [attr-renderer]]
    [hypercrud.ui.widget :as widget]))


(defn auto-control [ctx]
  {:post [(not (nil? %))]}
  (let [attr @(:hypercrud.browser/fat-attribute ctx)
        ;display-mode (-> @(:hypercrud.ui/display-mode ctx) name keyword)
        layout (-> (:hyperfiddle.ui/layout ctx :hyperfiddle.ui.layout/block) name keyword)
        valueType (-> attr :db/valueType :db/ident name keyword)
        cardinality (-> attr :db/cardinality :db/ident name keyword)
        isComponent (-> attr :db/isComponent (if :component))
        renderer (blank->nil (:attribute/renderer attr))]

    (r/partial
      portal-markup
      (match* [layout valueType cardinality isComponent renderer]

        [_ _ _ _ renderer] (attr-renderer renderer ctx)

        [:table :boolean :one _ _] widget/boolean
        [:table :keyword :one _ _] widget/keyword
        [:table :string :one _ _] widget/string
        [:table :long :one _ _] widget/long
        [:table :instant :one _ _] instant
        [:table :ref :one :component _] table-cell/ref-one-component
        [:table :ref :one nil _] widget/ref
        [:table :ref :many :component _] table-cell/ref-many
        [:table :ref :many nil _] edn-many

        ; :block vs :inline-block currently handled inside the widget, todo
        [_ :boolean :one _ _] widget/boolean
        [_ :keyword :one _ _] widget/keyword
        [_ :string :one _ _] widget/string
        [_ :long :one _ _] widget/long
        [_ :instant :one _ _] instant
        [_ :ref :one :component _] widget/ref-component
        [_ :ref :one nil _] widget/ref
        [_ :ref :many :component _] widget/ref-many-table
        [_ :ref :many nil _] edn-many

        ; Unmatched valueType, like #uri and #uuid
        [_ _ :many _ _] edn-many
        [_ _ :one _ _] edn

        [_ nil _ _ _] widget/text                           ; something is wrong
        ))))
