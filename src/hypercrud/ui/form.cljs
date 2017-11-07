(ns hypercrud.ui.form
  (:require [cuerdas.core :as str]
            [hypercrud.browser.auto-form :as auto-form]
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
                         (remove :anchor/repeating? $)      ; because we're in the label
                         (widget/process-option-anchors $ ctx))]
     [:div.hc-label
      [:label [form-util/field-label field ctx]]
      [:div.anchors
       (widget/render-anchors (->> anchors (remove :anchor/render-inline?)) ctx)
       (widget/render-inline-anchors (->> anchors (filter :anchor/render-inline?)) ctx)]])
   (control ctx)
   (let [docstring (util/fallback empty?
                                  (:db/doc field)           ; Nice string for end users. In future this is an i18n id.
                                  (-> ctx :attribute :db/doc) #_"english string for developers")]
     [markdown/markdown docstring #() {:class "hypercrud-doc"}])])

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

(defn Attribute [{:keys [:field/attribute] :as field} fe-anchors-lookup ctx]
  (let [ctx (as-> (context/attribute ctx attribute) $
                  (context/value $ (get-in ctx [:entity attribute]))
                  (if (= attribute :db/id) (assoc $ :read-only always-read-only) $))
        display-mode @(:display-mode ctx)
        ; What is the user-field allowed to change? The ctx. Can it change links or anchors? no.
        Field (case display-mode :xray Field :user (get ctx :field Field))
        Control (case display-mode :xray Control :user (get ctx :control Control))
        attr-anchors (get fe-anchors-lookup attribute)]
    ; todo control can have access to repeating contextual values (color, owner, result, entity, value, etc) but field should NOT
    ; this leads to inconsistent location formulas between non-repeating links in tables vs forms
    [Field (r/partial Control field attr-anchors) field attr-anchors ctx]))

(defn Entity [entity form fe-anchors-lookup ctx]
  (let [ctx (context/entity ctx entity)
        {inline-anchors true anchors false} (->> (get fe-anchors-lookup nil)
                                                 (filter :anchor/repeating?)
                                                 (group-by :anchor/render-inline?))
        splat? (or (empty? (:form/field form))
                   (->> (map :db/id (:form/field form))
                        (every? auto-form/system-field?)))]
    (concat
      (widget/render-anchors anchors ctx)
      (conj
        (->> (:form/field form)
             (mapv (fn [field]
                     ^{:key (:db/id field)}
                     [Attribute field fe-anchors-lookup ctx])))
        (if splat?
          ^{:key (hash (keys entity))}
          [new-field entity ctx]))
      (widget/render-inline-anchors inline-anchors ctx))))

(defn FindElement [{fe-name :find-element/name :as fe} anchors-lookup ctx]
  (let [ctx (context/find-element ctx fe)
        fe-anchors-lookup (get anchors-lookup fe-name)
        ; todo these fe non-repeating anchors should not have relation in its context
        {inline-anchors true anchors false} (->> (get fe-anchors-lookup nil)
                                                 (remove :anchor/repeating?)
                                                 (group-by :anchor/render-inline?))]
    (concat
      (widget/render-anchors anchors ctx)
      (Entity (get (:relation ctx) fe-name) (:find-element/form fe) fe-anchors-lookup ctx)
      (widget/render-inline-anchors inline-anchors ctx))))

(defn Relation [relation ordered-fes anchors-lookup ctx]
  (let [ctx (context/relation ctx relation)]
    (mapcat #(FindElement % anchors-lookup ctx) ordered-fes)))

(defn form [relation ordered-fes anchors ctx]
  (let [ctx (assoc ctx :layout (:layout ctx :block))
        anchors-lookup (->> (widget/process-popover-anchors anchors ctx)
                            (group-by (comp :find-element/name :anchor/find-element))
                            (util/map-values (partial group-by :anchor/attribute)))
        {inline-index-anchors true index-anchors false} (->> (get-in anchors-lookup [nil nil])
                                                             (group-by :anchor/render-inline?))
        index-ctx (dissoc ctx :isComponent)]
    [:div {:class (str "forms-list " (name (:layout ctx)))}
     (widget/render-anchors index-anchors index-ctx)
     (Relation relation ordered-fes anchors-lookup ctx)
     (widget/render-inline-anchors inline-index-anchors index-ctx)]))
