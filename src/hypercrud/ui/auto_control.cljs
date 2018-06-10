(ns hypercrud.ui.auto-control
  (:require
    [contrib.reactive :as r]
    [clojure.core.match :refer [match match*]]
    [hypercrud.ui.attribute.edn :as edn]
    [hypercrud.ui.attribute.instant :as instant]
    [contrib.string :refer [blank->nil]]
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
        (match* [layout valueType cardinality isComponent]

          [:table :boolean :one _] widget/boolean
          [:table :keyword :one _] widget/keyword
          [:table :string :one _] widget/string
          [:table :long :one _] widget/long
          [:table :instant :one _] instant/instant
          [:table :ref :one true] table-cell/ref-one-component
          [:table :ref :one false] widget/ref
          [:table :ref :many true] table-cell/ref-many
          [:table :ref :many false] edn/edn-many

          ; :block vs :inline-block currently handled inside the widget, todo
          [_ :boolean :one _] widget/boolean
          [_ :keyword :one _] widget/keyword
          [_ :string :one _] widget/string
          [_ :long :one _] widget/long
          [_ :instant :one _] instant/instant
          [_ :ref :one true] widget/ref-component
          [_ :ref :one false] widget/ref
          [_ :ref :many true] widget/ref-many-table
          [_ :ref :many false] edn/edn-many

          ; Unmatched valueType, like #uri and #uuid
          [_ _ :many _] edn/edn-many
          [_ _ :one _] edn/edn

          [_ nil _ _] widget/text                           ; something is wrong
          )))))
