(ns hyperfiddle.ui.hyper-controls
  (:require
    [contrib.reactive :as r]
    [contrib.reagent :refer [fragment]]
    [contrib.ui :refer [debounced]]
    [contrib.ui.tooltip :refer [tooltip-thick]]
    [hypercrud.browser.context :as context]
    [hypercrud.browser.field :as field]
    [hyperfiddle.data :as data]
    [hyperfiddle.tempid :refer [smart-entity-identifier]]
    #_[hyperfiddle.ui]
    [hyperfiddle.ui.util :refer [readonly->disabled writable-entity?]]
    [hyperfiddle.ui.docstring :refer [semantic-docstring]]))


(defn label-with-docs [label help-md props]
  [tooltip-thick (if help-md [:div.hyperfiddle.docstring [contrib.ui/markdown help-md]])
   (let [label-props (select-keys props [:on-click :class])] ; https://github.com/hyperfiddle/hyperfiddle/issues/511
     [:label.hyperfiddle label-props label (if help-md [:sup "†"])])])

(defn dbid-label [_ ctx & [props]]
  (fragment
    (when-let [label (some->> (:hypercrud.browser/field ctx) (r/fmap ::field/label) deref)]
      (label-with-docs label (semantic-docstring ctx) props))

    ; dbid links are at parent path, but we don't always have a parent #543
    (let [ctx (:hypercrud.browser/parent ctx)
          new (some-> ctx (data/select-here :hf/new))]
      (if new
        [hyperfiddle.ui/ui-from-link new ctx props "new"]))))

(defn attribute-label [_ ctx & [props]]
  (when-let [label (some->> (:hypercrud.browser/field ctx) (r/fmap ::field/label) deref)]
    (label-with-docs label (semantic-docstring ctx) props)))

(defn relation-label [_ ctx & [props]]
  (label-with-docs "*relation*" (semantic-docstring ctx) props))

(defn tuple-label [_ ctx & [props]]
  (label-with-docs "*tuple*" (semantic-docstring ctx) props))

(defn magic-new-label [_ ctx & props]
  (when-let [label (some->> (:hypercrud.browser/field ctx) (r/fmap ::field/label) deref)]
    (label-with-docs label (semantic-docstring ctx "Free-hand attribute entry") props)))

(defn -change! [state ctx #_ov v]
  (let [e @(r/fmap (r/partial smart-entity-identifier ctx)
                   (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data]))]
    (context/with-tx! ctx [[:db/add e @state v]])))

(defn magic-new [val ctx props]
  (let [state (r/atom nil)]
    (fn [val ctx props]
      (let [read-only (not @(r/track writable-entity? ctx))]
        [:div
         [contrib.ui/keyword (-> (assoc props
                                   :placeholder ":db/ident"
                                   :value @state
                                   :on-change (r/partial reset! state))
                                 readonly->disabled)]
         (let [props (-> (assoc props
                           :magic-new-mode true
                           :on-blur (r/partial -change! state ctx)
                           :read-only (let [_ [@state]] ; force reactions
                                        (or (nil? @state) read-only))
                           :placeholder (pr-str :gender/female))
                         readonly->disabled)]
           ; Uncontrolled widget on purpose i think
           ; Cardinality :many not needed, because as soon as we assoc one value, we rehydrate typed
           [contrib.ui/edn props])]))))
