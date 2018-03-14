(ns hypercrud.ui.form
  (:require [hypercrud.browser.context :as context]
            [hypercrud.browser.find-element :as find-element]
            [hypercrud.browser.link :as link]
            [hypercrud.ui.auto-control :refer [auto-control' control-props]]
            [hypercrud.ui.connection-color :as connection-color]
            [hypercrud.ui.control.link-controls :as link-controls]
            [hypercrud.ui.css :refer [css-slugify classes]]
            [hypercrud.ui.input :as input]
            [hypercrud.ui.label :refer [label]]
            [hypercrud.util.reactive :as reactive]
            [hypercrud.react.react-fragment :refer [react-fragment]]))


(defn new-field-state-container [ctx]
  (let [attr-ident (reactive/atom nil)]
    (fn [ctx]
      ;busted
      [:div.field {:style {:border-color (connection-color/connection-color ctx)}}
       [:div.hc-label
        [:label
         (let [on-change! #(reset! attr-ident %)]
           [input/keyword-input* @attr-ident on-change!])]]
       (let [on-change! #(let [tx [[:db/add (:db/id @(:cell-data ctx)) @attr-ident %]]]
                           ; todo cardinality many
                           ((:user-with! ctx) tx))
             props (if (nil? @attr-ident) {:read-only true})]
         [input/edn-input* nil on-change! props])])))

(defn new-field [ctx]
  ^{:key (hash (keys @(:cell-data ctx)))}
  [new-field-state-container ctx])

(def always-read-only (constantly true))

(defn ui-block-border-wrap [ctx class & children]
  [:div {:class (classes class "hyperfiddle-form-cell" (-> ctx :hypercrud.browser/attribute str css-slugify))
         :style {:border-color (connection-color/connection-color ctx)}}
   (apply react-fragment :_ children)])

(defn form-cell [control -field ctx & [class]]              ; safe to return nil or seq
  (let [path [(:fe-pos ctx) (:hypercrud.browser/attribute ctx)]]
    (ui-block-border-wrap
      ctx (str class " field")
      ; todo unsafe execution of user code: label
      [:div ((:label ctx (partial vector label)) -field ctx)
       (link-controls/render-nav-cmps path false ctx link/options-processor)
       (link-controls/render-inline-links path false ctx link/options-processor)]
      ; todo unsafe execution of user code: control
      [control -field (control-props ctx) ctx])))

(defn Cell [field ctx]
  (let [ctx (as-> (context/attribute ctx (:attribute field)) $
                  (context/value $ (reactive/fmap (:cell-data->value field) (:cell-data ctx)))
                  (if (or (nil? (:attribute field))
                          (= (:attribute field) :db/id))
                    (assoc $ :read-only always-read-only)
                    $))
        ; todo unsafe execution of user code: cell
        user-cell (case @(:hypercrud.ui/display-mode ctx) :xray form-cell (:cell ctx form-cell))]
    (assert @(:hypercrud.ui/display-mode ctx))
    [user-cell (auto-control' ctx) field ctx]))

(defn Entity [ctx]
  (let [path [(:fe-pos ctx)]]
    (concat
      (link-controls/render-nav-cmps path false ctx :class "hyperfiddle-link-entity-independent")
      (let [ctx (context/cell-data ctx)]
        (concat
          (conj
            (->> @(reactive/cursor (:hypercrud.browser/find-element ctx) [:fields])
                 ; just dereference the fields immediately
                 (mapv (fn [field]
                         ^{:key (:id field)}
                         [Cell field ctx])))
            (if @(reactive/cursor (:hypercrud.browser/find-element ctx) [:splat?])
              ^{:key :new-field}
              [new-field ctx]))
          (link-controls/render-nav-cmps path true ctx :class "hyperfiddle-link-entity-dependent")
          (link-controls/render-inline-links path true ctx)))
      (link-controls/render-inline-links path false ctx))))

(defn FindElement "Context manufactured from path on demand to late bind to relation"
  [ctx i]
  ; No wrapper div; it limits layout e.g. floating. The pyramid mapcats out to a flat list of dom elements that comprise the form
  ; This is not compatible with hiccup syntax; this is a fn
  (Entity (context/find-element ctx i)))

(defn Relation [ctx]
  (let [ctx (assoc ctx :layout (:layout ctx :block))]       ; first point in time we know we're a form? can this be removed?
    (->> (reactive/unsequence (:hypercrud.browser/ordered-fes ctx))
         (mapcat (fn [[fe i]]
                   (FindElement ctx i))))))
