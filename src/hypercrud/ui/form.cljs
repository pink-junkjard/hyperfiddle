(ns hypercrud.ui.form
  (:require [cuerdas.core :as str]
            [hypercrud.browser.connection-color :as connection-color]
            [hypercrud.browser.context :as context]
            [hypercrud.ui.auto-control :refer [auto-control]]
            [hypercrud.ui.form-util :as form-util]
            [hypercrud.ui.input :as input]
            [hypercrud.ui.markdown :as markdown]
            [hypercrud.ui.renderer :as renderer]
            [hypercrud.ui.widget :as widget]
            [hypercrud.util.core :as util]
            [reagent.core :as r]))

(defn Control [field anchors ctx]
  (let [props (form-util/build-props field anchors ctx)]
    (if (renderer/user-renderer ctx)
      (renderer/user-render field anchors props ctx)
      [auto-control field anchors props ctx])))

(defn Field [control field anchors ctx]
  [:div {:class (str/join " " ["field" (-> ctx :attribute :db/ident str form-util/css-slugify)])
         :style {:border-color (connection-color/connection-color (:uri ctx) ctx)}}
   (let [[anchors] (as-> anchors $
                         (remove :link/dependent? $)        ; because we're in the label
                         (widget/process-option-anchors $ ctx))]
     [:div.hc-label
      [:label [form-util/field-label field ctx]]
      [:div.anchors
       (widget/render-anchors (->> anchors (remove :link/render-inline?)) ctx)
       (widget/render-inline-anchors (->> anchors (filter :link/render-inline?)) ctx)]])
   (control ctx)
   [markdown/markdown (-> ctx :attribute :db/doc) #() {:class "hypercrud-doc"}]])

(defn new-field [entity ctx]
  (let [attr-ident (r/atom nil)]
    (fn [entity ctx]
      [:div.field {:style {:border-color (connection-color/connection-color (:uri ctx) ctx)}}
       [:div.hc-label
        [:label
         (let [on-change! #(reset! attr-ident %)]
           [input/keyword-input* @attr-ident on-change!])]]
       (let [on-change! #(let [tx [[:db/add (:db/id entity) @attr-ident %]]]
                           ; todo cardinality many
                           ((:user-with! ctx) tx))
             props (if (nil? @attr-ident) {:read-only true})]
         [input/edn-input* nil on-change! props])])))

(def always-read-only (constantly true))

(defn Attribute [field fe-anchors-lookup ctx]
  (let [ctx (as-> (context/attribute ctx (:attribute field)) $
                  (context/value $ ((:cell-data->value field) (:cell-data ctx)))
                  (if (or (nil? (:attribute field))
                          (= (:attribute field) :db/id))
                    (assoc $ :read-only always-read-only)
                    $))
        display-mode @(:display-mode ctx)
        ; What is the user-field allowed to change? The ctx. Can it change links or anchors? no.
        Field (case display-mode :xray Field :user (get ctx :field Field))
        Control (case display-mode :xray Control :user (get ctx :control Control))
        attr-anchors (get-in fe-anchors-lookup [(:attribute field) :links])]
    ; todo control can have access to repeating contextual values (color, owner, result, entity, value, etc) but field should NOT
    ; this leads to inconsistent location formulas between non-repeating links in tables vs forms
    [Field (r/partial Control field attr-anchors) field attr-anchors ctx]))

(defn cell-data-fields [fe cell-data fe-anchors-lookup ctx]
  (let [ctx (context/cell-data ctx cell-data)
        {inline-anchors true anchors false} (->> (get fe-anchors-lookup :links)
                                                 (filter :link/dependent?)
                                                 (group-by :link/render-inline?))]
    (concat
      (widget/render-anchors anchors ctx)
      (conj
        (->> (:fields fe)
             (mapv (fn [field]
                     ^{:key (:id field)}
                     [Attribute field fe-anchors-lookup ctx])))
        (if (:splat? fe)
          ^{:key (hash (keys cell-data))}
          [new-field cell-data ctx]))
      (widget/render-inline-anchors inline-anchors ctx))))

(defn result-cell [fe cell-data fe-anchors-lookup ctx]
  (let [{inline-anchors true anchors false} (->> (get fe-anchors-lookup :links)
                                                 (remove :link/dependent?)
                                                 (group-by :link/render-inline?))]
    (concat
      (widget/render-anchors anchors ctx)
      (cell-data-fields fe cell-data fe-anchors-lookup ctx)
      (widget/render-inline-anchors inline-anchors ctx))))

(defn Relation [relation ordered-fes anchors-lookup ctx]
  (let [ctx (assoc ctx :layout (:layout ctx :block))]
    [:div {:class (name (:layout ctx))}
     (->> ordered-fes
          (map-indexed (fn [fe-pos fe]
                         (let [cell-data (get relation fe-pos)
                               fe-anchors-lookup (get anchors-lookup fe-pos)
                               ctx (context/find-element ctx fe)]
                           (result-cell fe cell-data fe-anchors-lookup ctx))))
          (apply concat))]))
