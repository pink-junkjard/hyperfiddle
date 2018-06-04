(ns hypercrud.ui.form
  (:require [contrib.css :refer [css-slugify classes]]
            [contrib.reactive :as r]
            [contrib.reagent :refer [fragment]]
            [hypercrud.browser.context :as context]
            [hypercrud.browser.link :as link]
            [hypercrud.ui.auto-control :refer [auto-control control-props]]
            [hypercrud.ui.connection-color :as connection-color]
            [hypercrud.ui.control.link-controls :as link-controls]
            [contrib.ui.input :as input]
            [hypercrud.ui.label :refer [label]]))


(defn ui-block-border-wrap [ctx class & children]
  [:div {:class (classes class "hyperfiddle-form-cell" (-> ctx :hypercrud.browser/attribute str css-slugify))
         :style {:border-color (connection-color/connection-color ctx)}}
   (apply fragment :_ children)])

(defn new-field-state-container [ctx]
  (let [attr-ident (r/atom nil)]
    (fn [ctx]
      (let [missing-dbid (nil? @(r/cursor (:cell-data ctx) [:db/id]))]
        (ui-block-border-wrap
          ctx "field"
          [:div (let [on-change! #(reset! attr-ident %)]
                  [input/keyword-input* @attr-ident on-change! {:read-only missing-dbid
                                                                :placeholder ":task/title"}])]
          (let [on-change! #(let [id @(r/cursor (:cell-data ctx) [:db/id])]
                              ; todo cardinality many
                              ((:user-with! ctx) [[:db/add id @attr-ident %]]))
                props {:read-only (or missing-dbid (nil? @attr-ident))
                       :placeholder (pr-str "mow the lawn")}]
            [input/edn-input* nil on-change! props]))))))

(defn new-field [ctx]
  ^{:key (hash (keys @(:cell-data ctx)))}
  [new-field-state-container ctx])

(defn Field "Form fields are label AND value. Table fields are label OR value."
  ([ctx] (Field nil ctx nil))
  ([?f ctx props]                                           ; fiddle-src wants to fallback by passing nil here explicitly
   (assert @(:hypercrud.ui/display-mode ctx))
   (let [path [(:fe-pos ctx) (:hypercrud.browser/attribute ctx)]]
     (ui-block-border-wrap
       ctx (classes "field" (:class props) #_":class is for the control, these props came from !cell{}")
       [:div
        [label ctx]
        (link-controls/anchors path false ctx link/options-processor)
        (link-controls/iframes path false ctx link/options-processor)]
       ; todo unsafe execution of user code: control
       [(or ?f (auto-control ctx)) @(:value ctx) ctx (merge (control-props ctx) props)]))))

(defn FeFields "form labels AND values for a find-element" [ctx]
  (let [path [(:fe-pos ctx)]]
    (concat
      (link-controls/anchors path false ctx nil {:class "hyperfiddle-link-entity-independent"})
      (let [ctx (context/cell-data ctx)]
        (concat
          (conj
            (->> (r/cursor (:hypercrud.browser/find-element ctx) [:fields])
                 (r/unsequence :id)
                 (mapv (fn [[field id]]
                         (let [ctx (context/field ctx @field)
                               ctx (context/value ctx)]
                           ^{:key id} [Field ctx]))))
            (if @(r/cursor (:hypercrud.browser/find-element ctx) [:splat?])
              ^{:key :new-field}
              [new-field ctx]))
          (link-controls/anchors path true ctx nil {:class "hyperfiddle-link-entity-dependent"})
          (link-controls/iframes path true ctx)))
      (link-controls/iframes path false ctx))))

(defn Relation [ctx]
  ; No wrapper div; it limits layout e.g. floating. The pyramid mapcats out to a flat list of dom elements that comprise the form
  ; This is not compatible with hiccup syntax; this is a fn
  (let [ctx (assoc ctx :layout (:layout ctx :block))]       ; first point in time we know we're a form? can this be removed?
    (->> (r/unsequence (:hypercrud.browser/ordered-fes ctx))
         (mapcat (fn [[fe i]]
                   (FeFields (context/find-element ctx i)))))))
