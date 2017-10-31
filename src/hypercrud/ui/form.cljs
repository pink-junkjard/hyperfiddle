(ns hypercrud.ui.form
  (:require [hypercrud.browser.auto-form :as auto-form]
            [hypercrud.browser.connection-color :as connection-color]
            [hypercrud.browser.context :as context]
            [hypercrud.ui.auto-control :refer [auto-control]]
            [hypercrud.ui.form-util :as form-util]
            [hypercrud.ui.input :as input]
            [hypercrud.ui.renderer :as renderer]
            [hypercrud.ui.widget :as widget]
            [hypercrud.util.core :as util]
            [reagent.core :as r]
            [hypercrud.ui.markdown :as markdown]))

(defn Control [field anchors param-ctx]
  (let [props (form-util/build-props field anchors param-ctx)]
    (if (renderer/user-renderer param-ctx)
      (renderer/user-render field anchors props param-ctx)
      [auto-control field anchors props param-ctx])))

(defn Field [control field anchors param-ctx]
  [:div.field {:style {:border-color (connection-color/connection-color (:uri param-ctx) param-ctx)}}
   (let [[anchors] (as-> anchors $
                         (remove :anchor/repeating? $)      ; because we're in the label
                         (widget/process-option-anchors $ param-ctx))]
     [:div.hc-label
      [:label [form-util/field-label field param-ctx]]
      [:div.anchors
       (widget/render-anchors (->> anchors (remove :anchor/render-inline?)) param-ctx)
       (widget/render-inline-anchors (->> anchors (filter :anchor/render-inline?)) param-ctx)]])
   (control param-ctx)
   (let [docstring (util/fallback empty?
                                  (:db/doc field)           ; Nice string for end users. In future this is an i18n id.
                                  (-> param-ctx :attribute :db/doc) #_ "english string for developers")]
     [markdown/markdown docstring #() {:class "hypercrud-doc"}])])

(defn new-field [entity param-ctx]
  (let [attr-ident (r/atom nil)]
    (fn [entity param-ctx]
      [:div.field {:style {:border-color (connection-color/connection-color (:uri param-ctx) param-ctx)}}
       [:div.hc-label
        [:label
         (let [on-change! #(reset! attr-ident %)]
           [input/keyword-input* @attr-ident on-change!])]]
       (let [on-change! #(let [tx [[:db/add (:db/id entity) @attr-ident %]]]
                           ; todo cardinality many
                           ((:user-with! param-ctx) tx))
             props (if (nil? @attr-ident) {:read-only true})]
         [input/edn-input* nil on-change! props])])))

(def always-read-only (constantly true))

(defn Attribute [{:keys [:field/attribute] :as field} fe-anchors-lookup param-ctx]
  (let [param-ctx (as-> (context/attribute param-ctx attribute) $
                        (context/value $ (get-in param-ctx [:entity attribute]))
                        (if (= attribute :db/id) (assoc $ :read-only always-read-only) $))
        display-mode @(:display-mode param-ctx)
        ; What is the user-field allowed to change? The param-ctx. Can it change links or anchors? no.
        Field (case display-mode :xray Field :user (get param-ctx :field Field))
        Control (case display-mode :xray Control :user (get param-ctx :control Control))
        attr-anchors (get fe-anchors-lookup attribute)]
    ; todo control can have access to repeating contextual values (color, owner, result, entity, value, etc) but field should NOT
    ; this leads to inconsistent location formulas between non-repeating links in tables vs forms
    [Field (r/partial Control field attr-anchors) field attr-anchors param-ctx]))

(defn Entity [entity form fe-anchors-lookup param-ctx]
  (let [param-ctx (context/entity param-ctx entity)
        {inline-anchors true anchors false} (->> (get fe-anchors-lookup nil)
                                                 (filter :anchor/repeating?)
                                                 (group-by :anchor/render-inline?))
        splat? (or (empty? (:form/field form))
                   (->> (map :db/id (:form/field form))
                        (every? auto-form/system-field?)))]
    (concat
      (widget/render-anchors anchors param-ctx)
      (conj
        (->> (:form/field form)
             (mapv (fn [field]
                     ^{:key (:db/id field)}
                     [Attribute field fe-anchors-lookup param-ctx])))
        (if splat?
          ^{:key (hash (keys entity))}
          [new-field entity param-ctx]))
      (widget/render-inline-anchors inline-anchors param-ctx))))

(defn FindElement [{fe-name :find-element/name :as fe} anchors-lookup param-ctx]
  (let [param-ctx (context/find-element param-ctx fe)
        fe-anchors-lookup (get anchors-lookup fe-name)
        ; todo these fe non-repeating anchors should not have relation in its context
        {inline-anchors true anchors false} (->> (get fe-anchors-lookup nil)
                                                 (remove :anchor/repeating?)
                                                 (group-by :anchor/render-inline?))]
    (concat
      (widget/render-anchors anchors param-ctx)
      (Entity (get (:relation param-ctx) fe-name) (:find-element/form fe) fe-anchors-lookup param-ctx)
      (widget/render-inline-anchors inline-anchors param-ctx))))

(defn Relation [relation ordered-fes anchors-lookup param-ctx]
  (let [param-ctx (context/relation param-ctx relation)]
    (mapcat #(FindElement % anchors-lookup param-ctx) ordered-fes)))

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
