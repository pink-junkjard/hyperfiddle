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
(defn field [value maybe-field anchors param-ctx]
  [:div.field {:style {:border-color (connection-color (:color param-ctx))}}
   [:label
    (let [prompt (get maybe-field :field/prompt (-> param-ctx :attribute :attribute/ident str))
          docstring (-> maybe-field :field/attribute :attribute/doc)]
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
     (let [anchors (filter #(= (-> param-ctx :attribute :db/id) (some-> % :anchor/attribute :db/id)) anchors)]
       [auto-control value maybe-field anchors param-ctx]))])

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

; We can draw a form of a relation by flatting a list-of-forms
; Use the colspec abstraction to unify these cases (this means
; that entity results have been manually lifted into a relation
; with fe-name "entity")
(defn form [relation colspec anchors param-ctx]
  (let [top-anchors (->> anchors
                         (filter #(nil? (:anchor/find-element %)))
                         (filter #(nil? (:anchor/attribute %))))]
    [:div.forms-list
     (widget/render-anchors (concat
                              ; non-repeating should not have :result
                              (mapv vector
                                    (->> top-anchors
                                         (remove :anchor/render-inline?)
                                         (remove :anchor/repeating?))
                                    (repeat param-ctx))
                              ; repeating gets the :result in ctx
                              (mapv vector
                                    (->> top-anchors
                                         (remove :anchor/render-inline?)
                                         (filter :anchor/repeating?))
                                    (repeat (assoc param-ctx :result relation))))) ; todo :result -> :relation

     ; everything inside this let is repeating, thus getting :result in ctx
     (let [param-ctx (assoc param-ctx :result relation)     ;  todo :result -> :relation
           fe-anchors-lookup (->> anchors
                                  ; entity links can have attributes but not find-elements specified
                                  (filter #(or (:anchor/find-element %) (:anchor/attribute %)))
                                  (group-by (fn [anchor]
                                              (if-let [find-element (:anchor/find-element anchor)]
                                                (:find-element/name find-element)
                                                "entity"))))

           ;; Missing are find-element anchors (need to hoist and concat), its just one form now not a list-form
           spec-fields (->> (partition 3 colspec)
                            (mapv (fn [[fe-name ident maybe-field]]
                                    (let [entity (get relation fe-name)
                                          fe-anchors (get fe-anchors-lookup fe-name)
                                          param-ctx (as-> param-ctx $
                                                          (assoc $
                                                            :color ((:color-fn param-ctx) entity param-ctx)
                                                            :owner ((:owner-fn param-ctx) entity param-ctx)
                                                            :entity entity
                                                            ; :db/id is missing from schema so fake it here, it has no valueType
                                                            :attribute (get (:schema param-ctx) ident {:attribute/ident ident}))
                                                          (if (= ident :db/id) (assoc $ :read-only (constantly true)) $))
                                          v (get entity ident)]
                                      ^{:key (str ident)}
                                      [field v maybe-field anchors param-ctx]))))
           form? (->> (partition 3 colspec)
                      (mapv #(nth % 2))
                      (every? #(not= nil %)))
           extra (if-not form?
                   ; can we assert entity?
                   (if-let [entity (get relation "entity")] ; makes sense only for entity links, not query links as entity.
                     ^{:key (hash (keys entity))} [new-field entity param-ctx]))]

       (concat spec-fields [extra])

       #_(map (fn [find-element]
                (let [entity (get relation (:find-element/name find-element))
                      find-element-anchors (get fe-anchors-lookup (:find-element/name find-element))
                      param-ctx (assoc param-ctx :color ((:color-fn param-ctx) entity param-ctx)
                                                 :owner ((:owner-fn param-ctx) entity param-ctx)
                                                 :entity entity)]
                  ^{:key (str (:db/id entity) "-" (:find-element/name find-element))}
                  [:div.find-element
                   (widget/render-anchors (->> find-element-anchors
                                               (filter #(nil? (:anchor/attribute %)))
                                               (remove :anchor/render-inline?))
                                          param-ctx)
                   (let [form* (if (not= (:display-mode param-ctx) :raw)
                                 (:find-element/form find-element)
                                 ; Ignore form in raw mode; it might be over-hydrated
                                 nil)]
                     [form entity form* find-element-anchors param-ctx])
                   (widget/render-inline-links (->> find-element-anchors
                                                    (filter #(nil? (:anchor/attribute %)))
                                                    (filter :anchor/render-inline?))
                                               (dissoc param-ctx :isComponent))]))
              ordered-find-elements))
     (widget/render-inline-links (concat
                                   ; non-repeating doesn't get :result
                                   (let [param-ctx (dissoc param-ctx :isComponent)]
                                     (mapv vector
                                           (->> top-anchors
                                                (filter :anchor/render-inline?)
                                                (remove :anchor/repeating?))
                                           (repeat param-ctx)))
                                   ; repeating gets :result
                                   (let [param-ctx (-> param-ctx (dissoc :isComponent) (assoc :result relation))]
                                     (mapv vector
                                           (->> top-anchors
                                                (filter :anchor/render-inline?)
                                                (filter :anchor/repeating?))
                                           (repeat param-ctx)))))]))
