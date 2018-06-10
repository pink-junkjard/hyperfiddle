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
      (if renderer
        (attr-renderer renderer ctx)

        ; Matching on scalars for compile time performance.
        ; We can omit trailing params by matching a vector but it explodes compile times and/or OOM
        (match* [layout valueType cardinality isComponent]

          [:table :boolean :one _] widget/boolean
          [:table :keyword :one _] widget/keyword
          [:table :string :one _] widget/string
          [:table :long :one _] widget/long
          [:table :instant :one _] instant
          [:table :ref :one :component] table-cell/ref-one-component
          [:table :ref :one nil] widget/ref
          [:table :ref :many :component] table-cell/ref-many
          [:table :ref :many nil] edn-many

          ; :block vs :inline-block currently handled inside the widget, todo
          [_ :boolean :one _] widget/boolean
          [_ :keyword :one _] widget/keyword
          [_ :string :one _] widget/string
          [_ :long :one _] widget/long
          [_ :instant :one _] instant
          [_ :ref :one :component] widget/ref-component
          [_ :ref :one nil] widget/ref
          [_ :ref :many :component] widget/ref-many-table
          [_ :ref :many nil] edn-many

          ; Unmatched valueType, like #uri and #uuid
          [_ _ :many _] edn-many
          [_ _ :one _] edn

          [_ nil _ _] widget/text                           ; something is wrong
          )))))
