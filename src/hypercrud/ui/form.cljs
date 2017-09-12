(ns hypercrud.ui.form
  (:require [cats.core :refer [mlet]]
            [hypercrud.browser.connection-color :as connection-color]
            [hypercrud.browser.context :as context]
            [hypercrud.compile.eval :refer [eval-str']]
            [hypercrud.ui.auto-control :refer [auto-control]]
            [hypercrud.ui.form-util :as form-util]
            [hypercrud.ui.input :as input]
            [hypercrud.ui.renderer :as renderer]
            [hypercrud.ui.widget :as widget]
            [hypercrud.util.core :as util]
            [hypercrud.util.monad :refer [exception->either]]
            [reagent.core :as r]))

(defn control [maybe-field anchors param-ctx]
  (let [props (form-util/build-props maybe-field anchors param-ctx)]
    (if (renderer/user-renderer param-ctx)
      (renderer/user-render maybe-field anchors props param-ctx)
      [auto-control maybe-field anchors props param-ctx])))

(defn field [control maybe-field anchors param-ctx]
  [:div.field {:style {:border-color (connection-color/connection-color (:color param-ctx))}}
   (let [[anchors] (as-> anchors $
                         (remove :anchor/repeating? $)      ; because we're in the label
                         (widget/process-option-anchors $ param-ctx))]
     [:div.hc-label
      [:label (form-util/field-label maybe-field param-ctx)]
      [:div.anchors
       (widget/render-anchors (->> anchors (remove :anchor/render-inline?)) param-ctx)
       (widget/render-inline-anchors (->> anchors (filter :anchor/render-inline?)) param-ctx)]])
   (control param-ctx)])

(defn new-field [entity param-ctx]
  (let [attr-ident (r/atom nil)]
    (fn [entity param-ctx]
      [:div.field {:style {:border-color (connection-color/connection-color (-> entity :db/id :conn-id))}}
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

(defn Attribute [{:keys [attr maybe-field]} fe-anchors-lookup param-ctx]
  (let [ident (-> attr :db/ident)
        param-ctx (as-> (context/attribute param-ctx attr) $
                        (context/value $ (get-in param-ctx [:entity ident]))
                        (if (= ident :db/id) (assoc $ :read-only always-read-only) $))
        attr-anchors (get fe-anchors-lookup ident)
        ; What is the user-field allowed to change? The param-ctx. Can it change links or anchors? no.
        field (case (:display-mode param-ctx) :xray field :user (get param-ctx :field field))
        control (case (:display-mode param-ctx) :xray control :user (get param-ctx :control control))]
    ; todo control can have access to repeating contextual values (color, owner, result, entity, value, etc) but field should NOT
    ; this leads to inconsistent location formulas between non-repeating links in tables vs forms
    [field #(control maybe-field attr-anchors %) maybe-field attr-anchors param-ctx]))

(defn Entity [entity colspec fe-anchors-lookup param-ctx]
  (let [param-ctx (context/entity param-ctx entity)
        {inline-anchors true anchors false} (->> (get fe-anchors-lookup nil)
                                                 (filter :anchor/repeating?)
                                                 (group-by :anchor/render-inline?))
        splat? (->> (map :maybe-field colspec)
                    (every? nil?))]
    (concat
      (widget/render-anchors anchors param-ctx)
      (conj
        (->> colspec
             (mapv (fn [{:keys [attr maybe-field] :as col}]
                     ^{:key (or (:db/id maybe-field) (str (:db/ident attr)))}
                     [Attribute col fe-anchors-lookup param-ctx])))
        (if splat?
          ^{:key (hash (keys entity))}
          [new-field entity param-ctx]))
      (widget/render-inline-anchors inline-anchors param-ctx))))

(defn FindElement [[fe colspec] anchors-lookup param-ctx]
  (let [param-ctx (context/find-element param-ctx fe)
        fe-name (:find-element/name fe)
        fe-anchors-lookup (get anchors-lookup fe-name)
        ; todo these fe non-repeating anchors should not have relation in its context
        {inline-anchors true anchors false} (->> (get fe-anchors-lookup nil)
                                                 (remove :anchor/repeating?)
                                                 (group-by :anchor/render-inline?))]
    (concat
      (widget/render-anchors anchors param-ctx)
      (Entity (get (:result param-ctx) fe-name) colspec fe-anchors-lookup param-ctx)
      (widget/render-inline-anchors inline-anchors param-ctx))))

(defn Relation [relation colspec anchors-lookup param-ctx]
  (let [param-ctx (context/relation param-ctx relation)]
    (->> (group-by :fe colspec)
         (mapcat #(FindElement % anchors-lookup param-ctx)))))

(defn form [relation colspec anchors ctx]
  (let [ctx (assoc ctx :layout (:layout ctx :block))
        anchors-lookup (->> (widget/process-popover-anchors anchors ctx)
                            (group-by (comp :find-element/name :anchor/find-element))
                            (util/map-values (partial group-by :anchor/attribute)))
        {inline-index-anchors true index-anchors false} (->> (get-in anchors-lookup [nil nil])
                                                             (group-by :anchor/render-inline?))
        index-ctx (dissoc ctx :isComponent)]
    [:div {:class (str "forms-list " (name (:layout ctx)))}
     (widget/render-anchors index-anchors index-ctx)
     (Relation relation colspec anchors-lookup ctx)
     (widget/render-inline-anchors inline-index-anchors index-ctx)]))
