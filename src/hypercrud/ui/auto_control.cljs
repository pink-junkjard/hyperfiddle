(ns hypercrud.ui.auto-control
  (:require
    [contrib.reactive :as r]
    [contrib.reagent :refer [fragment]]
    [contrib.string :refer [blank->nil]]
    [clojure.core.match :refer [match match*]]
    [hypercrud.browser.link :refer [options-processor options-link]]
    [hypercrud.ui.attribute.edn :refer [edn edn-many]]
    [hypercrud.ui.attribute.instant :refer [instant]]
    [hypercrud.ui.control.link-controls :refer [anchors iframes]]
    ;[hypercrud.ui.form :as form]
    [hypercrud.ui.label :refer [auto-label]]
    ;[hypercrud.ui.table :as table]
    [hypercrud.ui.safe-render :refer [portal-markup]]
    [hypercrud.ui.table-cell :as table-cell]
    [hypercrud.ui.util :refer [attr-renderer]]
    [hypercrud.ui.widget :as widget]))


(defn control [ctx]
  (let [layout (-> (:hyperfiddle.ui/layout ctx :hyperfiddle.ui.layout/block) name keyword)
        a (:hypercrud.browser/attribute ctx)
        attr (some-> ctx :hypercrud.browser/fat-attribute deref)
        ;display-mode (-> @(:hypercrud.ui/display-mode ctx) name keyword)
        type (some-> attr :db/valueType :db/ident name keyword)
        cardinality (some-> attr :db/cardinality :db/ident name keyword)
        isComponent (some-> attr :db/isComponent (if :component))
        renderer (blank->nil (:attribute/renderer attr))]

    (r/partial
      portal-markup
      (if renderer
        (attr-renderer renderer ctx)
        (match [type cardinality]
          [:boolean :one] widget/boolean
          [:keyword :one] widget/keyword
          [:string :one] widget/string
          [:long :one] widget/long
          [:instant :one] instant
          [:ref :one] widget/dbid                       ; nested form?
          [:ref :many] edn-many                         ; nested table?
          [_ :one] edn
          [_ :many] edn-many
          )))))

(defn auto-control "hyper-control" [ctx]
  {:post [(not (nil? %))]}

  ; Look for an :options link for select renderer.
  (if (options-link ctx)
    widget/select                                           ; hypermedia control

    (let [layout (-> (:hyperfiddle.ui/layout ctx :hyperfiddle.ui.layout/block) name keyword)
          dependent (-> (:relation ctx) (if :body :head))
          i (:fe-pos ctx)
          a (:hypercrud.browser/attribute ctx)]

      (if (= a '*)
        (fn [field ctx props] [:code "magic-new"])
        (match [dependent i a]

          [:head i a] (fn [field ctx props]
                        (fragment (auto-label field ctx props)
                                  (anchors :head i a ctx nil)
                                  (iframes :head i a ctx nil)))
          [:body i a] (fn [value ctx props]
                        (fragment (if a ((control ctx) value ctx props))
                                  (anchors :body i a ctx nil)
                                  (iframes :body i a ctx nil)))
          [:head i nil] (fn [field ctx props]
                          (fragment (auto-label field ctx props)
                                    (anchors :head i nil ctx nil)
                                    (iframes :head i nil ctx nil)))
          [:body i nil] (fn [value ctx props]
                          ; element values are allowed with custom renderer
                          (fragment (anchors :body i nil ctx nil)
                                    (iframes :body i nil ctx nil)))
          [:head nil nil] (fn [field ctx props]
                            (fragment (anchors :head nil nil ctx nil)
                                      (iframes :head nil nil ctx nil)))
          [:body nil nil] (fn [value ctx props]
                            [:code "body naked render only sensible with custom renderer"])
          )))))

(comment
  ; Deal with magic-new-field up here?
  [:block :head i '*] (fn [field ctx props] [:code "form head magic-new"])
  [:block :body i '*] (fn [value ctx props] [:code "form body magic-new"])
  [:table :head i '*] (fn [field ctx props] [:code "table head magic-new"])
  [:table :body i '*] (fn [value ctx props] [:code "table body magic-new"])
  )