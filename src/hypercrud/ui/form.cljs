(ns hypercrud.ui.form
  (:require [hypercrud.browser.links :as links]
            [hypercrud.compile.eval :refer [eval]]
            [hypercrud.types :refer [->DbVal]]
            [hypercrud.ui.auto-control :refer [auto-control connection-color]]
            [hypercrud.ui.renderer :as renderer]))


(defn field [entity {:keys [:field/prompt] :as field} link-ctxs {:keys [peer] :as param-ctx}]
  [:div.field
   [:label
    (let [docstring (-> field :field/attribute :attribute/doc)]
      (if-not (empty? docstring)
        [:span.help {:on-click #(js/alert docstring)} prompt]
        prompt))]
   (if-let [renderer (renderer/renderer-for-attribute (:field/attribute field))]
     (let [{renderer :value error :error} (eval renderer)
           repeating-link-ctxs (->> link-ctxs
                                    (filter :link-ctx/repeating?)
                                    (mapv (juxt #(-> % :link-ctx/ident) identity))
                                    (into {}))
           link-fn (fn [ident label param-ctx]
                     (let [link-ctx (get repeating-link-ctxs ident)
                           props (links/build-link-props link-ctx param-ctx)]
                       [(:navigate-cmp param-ctx) props label param-ctx]))]
       [:div.value
        (if error
          (pr-str error)
          (try
            (renderer peer link-fn entity)
            (catch :default e (pr-str e))))])
     (let [link-ctxs (filter #(= (:db/id field) (some-> % :link-ctx/field :db/id)) link-ctxs)]
       [auto-control entity field link-ctxs param-ctx]))])


(defn form [entity form link-ctxs param-ctx]
  (let [param-ctx (assoc param-ctx :color ((:color-fn param-ctx) entity param-ctx)
                                   :owner ((:owner-fn param-ctx) entity param-ctx))
        style {:border-color (connection-color (:color param-ctx))}]
    [:div.form {:style style}
     (->> (:form/field form)
          (sort-by :field/order)
          (map (fn [fieldinfo]
                 (let [ident (-> fieldinfo :field/attribute :attribute/ident)]
                   ^{:key ident}
                   [field entity fieldinfo link-ctxs param-ctx]))))]))
