(ns hypercrud.ui.auto-control
  (:require
    [contrib.reactive :as r]
    [contrib.reagent :refer [fragment]]
    [contrib.string :refer [blank->nil]]
    [clojure.core.match :refer [match match*]]
    [hypercrud.browser.link :refer [links-here rel->link]]
    [hypercrud.ui.attribute.edn :refer [edn edn-many]]
    [hypercrud.ui.attribute.instant :refer [instant]]
    [hypercrud.ui.control.link-controls :refer [anchors iframes]]
    ;[hypercrud.ui.form :as form]
    [hypercrud.ui.label :refer [label]]
    ;[hypercrud.ui.table :as table]
    [hypercrud.ui.safe-render :refer [portal-markup]]
    [hypercrud.ui.util :refer [attr-renderer]]
    [hypercrud.ui.widget :as widget]))


(defn control [ctx]
  (let [layout (-> (:hyperfiddle.ui/layout ctx :hyperfiddle.ui.layout/block) name keyword)
        a (:hypercrud.browser/attribute ctx)
        attr (some-> ctx :hypercrud.browser/fat-attribute deref)
        display-mode (-> @(:hypercrud.ui/display-mode ctx) name keyword)
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
          [:ref :one] widget/dbid                           ; nested form?
          [:ref :many] edn-many                             ; nested table?
          [_ :one] edn
          [_ :many] edn-many
          )))))

(defn auto-control "hyper-control" [ctx]
  {:post [(not (nil? %))]}

  (let [display-mode (-> @(:hypercrud.ui/display-mode ctx) name keyword)
        layout (-> (:hyperfiddle.ui/layout ctx :hyperfiddle.ui.layout/block) name keyword)
        dependent (-> (:relation ctx) (if :body :head))
        i (:fe-pos ctx)
        a (:hypercrud.browser/attribute ctx)
        rels (->> (links-here ctx) (map :link/rel) (into #{}))]

    (if (= a '*)
      (fn [field ctx props] [:code "magic-new"])
      (match [dependent i a rels]

        [:body i a (true :<< #(contains? % :options))] widget/select
        [:head i a (true :<< #(contains? % :options))] (fn [field ctx props]
                                                         (fragment (if i (label field ctx props))
                                                                   (case display-mode
                                                                     :user nil
                                                                     :xray
                                                                     (if-not (-> (rel->link :options ctx) :link/dependent?)
                                                                       [widget/select nil ctx (assoc props :disabled true)])
                                                                     )))
        [:head i a _] (fn [field ctx props]
                        (fragment (if i (label field ctx props))
                                  (anchors :head i a ctx nil)
                                  (iframes :head i a ctx nil)))
        [:body i a _] (fn [value ctx props]
                        (fragment (if a ((control ctx) value ctx props))
                                  (if i (anchors :body i a ctx nil))
                                  (if i (iframes :body i a ctx nil))))
        ))))

(comment
  ; Deal with magic-new-field up here?
  [:block :head i '*] (fn [field ctx props] [:code "form head magic-new"])
  [:block :body i '*] (fn [value ctx props] [:code "form body magic-new"])
  [:table :head i '*] (fn [field ctx props] [:code "table head magic-new"])
  [:table :body i '*] (fn [value ctx props] [:code "table body magic-new"])
  )