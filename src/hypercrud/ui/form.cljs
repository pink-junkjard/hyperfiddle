(ns hypercrud.ui.form
  (:require [hypercrud.browser.links :as links]
            [hypercrud.compile.eval :refer [eval]]
            [hypercrud.types :refer [->DbVal]]
            [hypercrud.ui.auto-control :refer [auto-control connection-color]]
            [hypercrud.ui.renderer :as renderer]))


(defn field [entity {:keys [:field/prompt] :as field} anchors {:keys [peer] :as param-ctx}]
  [:div.field
   [:label
    (let [docstring (-> field :field/attribute :attribute/doc)]
      (if-not (empty? docstring)
        [:span.help {:on-click #(js/alert docstring)} prompt]
        prompt))]
   (if-let [renderer (renderer/renderer-for-attribute (:field/attribute field))]
     (let [{renderer :value error :error} (eval renderer)
           repeating-anchors (->> anchors
                                  (filter :anchor/repeating?)
                                  (mapv (juxt #(-> % :anchor/ident) identity))
                                  (into {}))
           link-fn (fn [ident label param-ctx]
                     (let [anchor (get repeating-anchors ident)
                           props (links/build-link-props anchor param-ctx)]
                       [(:navigate-cmp param-ctx) props label param-ctx]))]
       [:div.value
        (if error
          (pr-str error)
          (try
            (renderer peer link-fn entity)
            (catch :default e (pr-str e))))])
     (let [anchors (filter #(= (:db/id field) (some-> % :anchor/field :db/id)) anchors)]
       [auto-control entity field anchors param-ctx]))])


(defn form [entity form anchors param-ctx]
  (let [param-ctx (assoc param-ctx :color ((:color-fn param-ctx) entity param-ctx)
                                   :owner ((:owner-fn param-ctx) entity param-ctx)
                                   :entity entity)
        style {:border-color (connection-color (:color param-ctx))}]
    [:div.form {:style style}
     (->> (:form/field form)
          (sort-by :field/order)
          (map (fn [fieldinfo]
                 ^{:key (:db/id fieldinfo)}
                 [field entity fieldinfo anchors param-ctx])))]))
