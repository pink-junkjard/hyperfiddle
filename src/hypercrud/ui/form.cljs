(ns hypercrud.ui.form
  (:require [hypercrud.browser.links :as links]
            [hypercrud.compile.eval :refer [eval]]
            [hypercrud.types :refer [->DbVal]]
            [hypercrud.ui.auto-control :refer [auto-control raw-control connection-color]]
            [hypercrud.ui.input :as input]
            [hypercrud.ui.renderer :as renderer]
            [hypercrud.ui.widget :as widget]
            [reagent.core :as r]
            [hypercrud.ui.form-util :as form-util]))


; field is optional (raw mode); schema and attribute is in dynamic scope in all modes
(defn field [value field anchors param-ctx]
  [:div.field {:style {:border-color (connection-color (:color param-ctx))}}
   [:label
    (let [prompt (get field :field/prompt (-> param-ctx :attribute :attribute/ident str))
          docstring (-> field :field/attribute :attribute/doc)]
      (if-not (empty? docstring)
        [:span.help {:on-click #(js/alert docstring)} prompt]
        prompt))]
   (if-let [renderer (renderer/renderer-for-attribute (:attribute param-ctx))]
     (let [{renderer :value error :error} (eval renderer)
           anchor-lookup (->> anchors
                              (mapv (juxt #(-> % :anchor/ident) identity))
                              (into {}))
           link-fn (fn [ident label param-ctx]
                     (let [anchor (get anchor-lookup ident)
                           props (links/build-link-props anchor param-ctx)]
                       [(:navigate-cmp param-ctx) props label param-ctx]))]
       [:div.value
        (if error
          (pr-str error)
          (try
            (renderer (:peer param-ctx) link-fn value)
            (catch :default e (pr-str e))))])
     (let [anchors (filter #(= (:db/id field) (some-> % :anchor/field :db/id)) anchors)]
       (if (= (:display-mode param-ctx) :raw)
         [raw-control value anchors param-ctx]
         [auto-control value field anchors param-ctx])))])


(defn new-field [entity param-ctx]
  (let [attr-ident (r/atom nil)]
    (fn [entity param-ctx]
      [:div.field
       [:label
        (let [on-change! #(reset! attr-ident %)]
          [input/keyword-input* @attr-ident on-change!])]
       (let [on-change! #(let [tx [[:db/add (:db/id entity) @attr-ident %]]]
                           ; todo cardinality many
                           ((:user-swap! param-ctx) {:tx tx}))
             props (if (nil? @attr-ident) {:read-only true})]
         [input/edn-input* nil on-change! props])])))


; don't pass a form if raw mode
(defn form [entity form anchors param-ctx]
  (let [style {:border-color (connection-color (:color param-ctx))}
        indexed-fields (group-by (comp :attribute/ident :field/attribute) (:form/field form))]
    [:div.form {:style style}
     (->> entity
          (map (fn [[k v]]
                 ^{:key k}
                 [field v (get indexed-fields k) anchors (assoc param-ctx :attribute (get (:schema param-ctx) k))]))
          (doall))
     (if (= (:display-mode param-ctx) :raw)
       ^{:key (hash (keys entity))}                         ; reset local state
       [new-field entity param-ctx])]))


(defn forms-list [resultset ordered-find-elements anchors param-ctx]
  (let [top-anchors (->> anchors
                         (filter #(nil? (:anchor/find-element %)))
                         (filter #(nil? (:anchor/field %))))
        colspec (form-util/determine-colspec resultset ordered-find-elements param-ctx)]
    [:div.forms-list
     (widget/render-anchors (concat
                              ; non-repeating should not have :result
                              (mapv vector
                                    (->> top-anchors
                                         (remove :anchor/render-inline?)
                                         (remove :anchor/repeating?))
                                    (repeatedly (constantly param-ctx)))
                              (if-let [result (first resultset)]
                                ; repeating gets the :result in ctx
                                (mapv vector
                                      (->> top-anchors
                                           (remove :anchor/render-inline?)
                                           (filter :anchor/repeating?))
                                      (repeatedly (constantly (assoc param-ctx :result result)))))))
     (if-let [result (first resultset)]
       ; everything inside this let is repeating, thus getting :result in ctx
       (let [param-ctx (assoc param-ctx :result result)
             fe-anchors-lookup (->> anchors
                                    ; entity links can have fields but not find-elements specified
                                    (filter #(or (:anchor/find-element %) (:anchor/field %)))
                                    (group-by (fn [anchor]
                                                (if-let [find-element (:anchor/find-element anchor)]
                                                  (:find-element/name find-element)
                                                  "entity"))))]

         ;; Missing are find-element anchors (need to hoist and concat), its just one form now not a list-form

         (doall
           (->> (partition 3 colspec)
                (map (fn [[fe-name ident maybe-field]]
                       (let [entity (get result fe-name)
                             fe-anchors (get fe-anchors-lookup fe-name)
                             param-ctx (assoc param-ctx :color ((:color-fn param-ctx) entity param-ctx)
                                                        :owner ((:owner-fn param-ctx) entity param-ctx)
                                                        :entity entity
                                                        :attribute (get (:schema param-ctx) ident))
                             v (get entity ident)]
                         ^{:key (str ident)}
                         [field v maybe-field anchors param-ctx]))))
           #_ (if (= (:display-mode param-ctx) :raw)
             ^{:key (hash colspec)}                         ; reset local state
             [new-field entity param-ctx])


           #_(map (fn [find-element]
                    (let [entity (get result (:find-element/name find-element))
                          find-element-anchors (get fe-anchors-lookup (:find-element/name find-element))
                          param-ctx (assoc param-ctx :color ((:color-fn param-ctx) entity param-ctx)
                                                     :owner ((:owner-fn param-ctx) entity param-ctx)
                                                     :entity entity)]
                      ^{:key (str (:db/id entity) "-" (:find-element/name find-element))}
                      [:div.find-element
                       (widget/render-anchors (->> find-element-anchors
                                                   (filter #(nil? (:anchor/field %)))
                                                   (remove :anchor/render-inline?))
                                              param-ctx)
                       (let [form* (if (not= (:display-mode param-ctx) :raw)
                                     (:find-element/form find-element)
                                     ; Ignore form in raw mode; it might be over-hydrated
                                     nil)]
                         [form entity form* find-element-anchors param-ctx])
                       (widget/render-inline-links (->> find-element-anchors
                                                        (filter #(nil? (:anchor/field %)))
                                                        (filter :anchor/render-inline?))
                                                   (dissoc param-ctx :isComponent))]))
                  ordered-find-elements)))
       [:div "No results"])
     (widget/render-inline-links (concat
                                   ; non-repeating should not have :result
                                   (let [param-ctx (dissoc param-ctx :isComponent)]
                                     (mapv vector
                                           (->> top-anchors
                                                (filter :anchor/render-inline?)
                                                (remove :anchor/repeating?))
                                           (repeatedly (constantly param-ctx))))
                                   (if-let [result (first resultset)]
                                     ; repeating gets the :result in ctx
                                     (let [param-ctx (-> param-ctx
                                                         (dissoc :isComponent)
                                                         (assoc :result result))]
                                       (mapv vector
                                             (->> top-anchors
                                                  (filter :anchor/render-inline?)
                                                  (filter :anchor/repeating?))
                                             (repeatedly (constantly param-ctx)))))))]))
