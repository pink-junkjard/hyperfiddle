(ns hypercrud.ui.form
  (:require [hypercrud.browser.links :as links]
            [hypercrud.compile.eval :refer [eval]]
            [hypercrud.types :refer [->DbVal]]
            [hypercrud.ui.auto-control :refer [auto-control connection-color]]))


(defn field [entity {:keys [:field/prompt :field/renderer] :as field} links {:keys [super-graph] :as param-ctx}]
  [:div.field
   [:label
    (let [docstring (-> field :field/attribute :attribute/doc)]
      (if-not (empty? docstring)
        [:span.help {:on-click #(js/alert docstring)} prompt]
        prompt))]
   (if (empty? renderer)
     [auto-control entity field links param-ctx]
     (let [{renderer :value error :error} (eval renderer)
           repeating-links (->> links
                                (filter :link/repeating?)
                                (mapv (juxt :link/ident identity))
                                (into {}))
           link-fn (fn [ident label]
                     (let [link (get repeating-links ident)
                           props (links/query-link link param-ctx)]
                       [(:navigate-cmp param-ctx) props label param-ctx]))]
       [:div.value
        (if error
          (pr-str error)
          (try
            (renderer super-graph link-fn entity)
            (catch :default e (pr-str e))))]))])


(defn form [entity form links param-ctx]
  (let [param-ctx (assoc param-ctx :color ((:color-fn param-ctx) entity param-ctx)
                                   :owner ((:owner-fn param-ctx) entity param-ctx))
        style {:border-color (connection-color (:color param-ctx))}]
    [:div.form {:style style}
     (->> (:form/field form)
          (sort-by :field/order)
          (map (fn [fieldinfo]
                 (let [ident (-> fieldinfo :field/attribute :attribute/ident)]
                   ^{:key ident}
                   [field entity fieldinfo links param-ctx]))))]))


(defn form-pull-exp [form]
  (concat
    [:db/id]
    (remove nil? (mapv #(-> % :field/attribute :attribute/ident) (:form/field form)))))


(defn query-pull-exp [find-elements]
  (->> find-elements
       (mapv (juxt :find-element/name (fn [{:keys [:find-element/connection :find-element/form]}]
                                        [(->DbVal (-> connection :db/id :id) nil) (form-pull-exp form)])))
       (into {})))
