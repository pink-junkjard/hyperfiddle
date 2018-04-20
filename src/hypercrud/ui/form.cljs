(ns hypercrud.ui.form
  (:require [contrib.css :refer [css-slugify classes]]
            [contrib.reactive :as r]
            [contrib.reagent :refer [fragment]]
            [hypercrud.browser.context :as context]
            [hypercrud.browser.link :as link]
            [hypercrud.ui.auto-control :refer [auto-control' control-props]]
            [hypercrud.ui.connection-color :as connection-color]
            [hypercrud.ui.control.link-controls :as link-controls]
            [hypercrud.ui.input :as input]
            [hypercrud.ui.label :refer [label]]))


(defn ui-block-border-wrap [ctx class & children]
  [:div {:class (classes class "hyperfiddle-form-cell" (-> ctx :hypercrud.browser/attribute str css-slugify))
         :style {:border-color (connection-color/connection-color ctx)}}
   (apply fragment :_ children)])

(defn new-field-state-container [ctx]
  (let [attr-ident (r/atom nil)]
    (fn [ctx]
      (ui-block-border-wrap
        ctx "field"
        [:div (let [on-change! #(reset! attr-ident %)]
                [input/keyword-input* @attr-ident on-change!])]
        (let [on-change! #(let [tx [[:db/add (:db/id @(:cell-data ctx)) @attr-ident %]]]
                            ; todo cardinality many
                            ((:user-with! ctx) tx))
              props nil #_(if (nil? @attr-ident) {:read-only true})]
          [input/edn-input* nil on-change! props])))))

(defn new-field [ctx]
  ^{:key (hash (keys @(:cell-data ctx)))}
  [new-field-state-container ctx])

(defn form-cell [control -field ctx & [class]]              ; safe to return nil or seq
  (let [path [(:fe-pos ctx) (:hypercrud.browser/attribute ctx)]]
    (ui-block-border-wrap
      ctx (str class " field")
      ; todo unsafe execution of user code: label
      [:div ((:label ctx (partial vector label)) -field ctx)
       (link-controls/anchors path false ctx link/options-processor)
       (link-controls/iframes path false ctx link/options-processor)]
      ; todo unsafe execution of user code: control
      [control -field (control-props ctx) ctx])))

(defn Cell [ctx]
  (assert @(:hypercrud.ui/display-mode ctx))
  ; todo unsafe execution of user code: cell[ctx i]
  (let [user-cell (case @(:hypercrud.ui/display-mode ctx) :xray form-cell (:hypercrud.browser/cell ctx form-cell))]
    [user-cell (auto-control' ctx) (:hypercrud.browser/field ctx) ctx]))

(defn Entity [ctx]
  (let [path [(:fe-pos ctx)]]
    (concat
      (link-controls/anchors path false ctx :class "hyperfiddle-link-entity-independent")
      (let [ctx (context/cell-data ctx)]
        (concat
          (conj
            (->> (r/cursor (:hypercrud.browser/find-element ctx) [:fields])
                 (r/unsequence :id)
                 (mapv (fn [[field id]]
                         ; unify with context/relation-path then remove
                         (let [field @field
                               ctx (as-> (context/field ctx field) $
                                         (context/value $ (r/fmap (:cell-data->value field) (:cell-data ctx)))
                                         (if (or (nil? (:attribute field)) (= (:attribute field) :db/id))
                                           (assoc $ :read-only (r/constantly true))
                                           $))]
                           ^{:key id}
                           [Cell ctx]))))
            (if @(r/cursor (:hypercrud.browser/find-element ctx) [:splat?])
              ^{:key :new-field}
              [new-field ctx]))
          (link-controls/anchors path true ctx :class "hyperfiddle-link-entity-dependent")
          (link-controls/iframes path true ctx)))
      (link-controls/iframes path false ctx))))

(defn Relation [ctx]
  ; No wrapper div; it limits layout e.g. floating. The pyramid mapcats out to a flat list of dom elements that comprise the form
  ; This is not compatible with hiccup syntax; this is a fn
  (let [ctx (assoc ctx :layout (:layout ctx :block))]       ; first point in time we know we're a form? can this be removed?
    (->> (r/unsequence (:hypercrud.browser/ordered-fes ctx))
         (mapcat (fn [[fe i]]
                   (Entity (context/find-element ctx i)))))))
