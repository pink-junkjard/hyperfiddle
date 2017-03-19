(ns hypercrud.ui.form
  (:require [cursor.core :refer [cursor]]
            [hypercrud.types :refer [->DbVal]]
            [hypercrud.browser.connection-color :as connection-color]
            [hypercrud.ui.auto-control :refer [auto-control]]
            [hypercrud.ui.form-util :as form-util]
            [hypercrud.ui.input :as input]
            [hypercrud.ui.renderer :as renderer]
            [hypercrud.ui.widget :as widget]
            [hypercrud.ui.code-editor :as code-editor]
            [reagent.core :as r]
            [re-com.core :as re-com]
            [hypercrud.util :as util]))

; field is optional (raw mode); schema and attribute is in dynamic scope in all modes
(defn field [value maybe-field anchors param-ctx]
  (let [tooltip-state (r/atom {:docstring false :raw false})]
    (fn [value maybe-field anchors param-ctx]
      (let [param-ctx (assoc param-ctx :value value)]
        [:div.field {:style {:border-color (connection-color/connection-color (:color param-ctx))}}
         [:label
          (let [tooltip-cur (cursor tooltip-state)
                docstring (or (-> maybe-field :field/doc) "")
                field-prompt (util/fallback empty? (get maybe-field :field/prompt) (-> param-ctx :attribute :attribute/ident str))]
            [:div
             (let [is-ref? (coll? value)]
               (if is-ref?
                 [re-com/popover-anchor-wrapper
                  :showing? (tooltip-cur [:raw])
                  :position :below-center
                  :anchor [:a {:href "javascript:void 0;" :on-click #(swap! (tooltip-cur [:raw]) not)} "§"]
                  :popover [re-com/popover-content-wrapper
                            :on-cancel #(do (reset! (tooltip-cur [:raw]) false) nil)
                            :no-clip? true
                            :body [code-editor/code-editor* (util/pprint-str value 100) nil {:readOnly true}]]]))
             " "
             [re-com/popover-tooltip
              :label docstring
              :showing? (tooltip-cur [:docstring])
              :anchor (let [props {:on-mouse-over #(do (if-not (empty? docstring) (reset! (tooltip-cur [:docstring]) true)) nil)
                                   :on-mouse-out #(do (reset! (tooltip-cur [:docstring]) false) nil)
                                   :class (if-not (empty? docstring) "help")}]
                        [:span props field-prompt])]])]

         (let [anchors (filter #(= (-> param-ctx :attribute :db/id) (some-> % :anchor/attribute :db/id)) anchors)
               props (form-util/build-props value maybe-field anchors param-ctx)]
           (if (renderer/user-renderer param-ctx)
             (renderer/user-render value maybe-field anchors props param-ctx)
             [auto-control value maybe-field anchors props param-ctx]))]))))

(defn new-field [entity param-ctx]
  (let [attr-ident (r/atom nil)]
    (fn [entity param-ctx]
      [:div.field {:style {:border-color (connection-color/connection-color (-> entity :db/id :conn-id))}}
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
           spec-fields (->> (partition 4 colspec)
                            (mapv (fn [[conn fe-name ident maybe-field]]
                                    (let [entity (get relation fe-name)
                                          fe-anchors (get fe-anchors-lookup fe-name)
                                          param-ctx (as-> param-ctx $
                                                          (form-util/entity-param-ctx entity $)
                                                          ; :db/id is missing from schema so fake it here, it has no valueType
                                                          (assoc $ :attribute (get (:schema param-ctx) ident {:attribute/ident ident})
                                                                   :layout :form)
                                                          (if (= ident :db/id) (assoc $ :read-only (constantly true)) $))
                                          v (get entity ident)]
                                      ^{:key (str ident)}
                                      [field v maybe-field anchors param-ctx]))))
           form? (->> (partition 4 colspec)
                      (mapv (fn [[conn fe-name attr maybe-field]] maybe-field))
                      (every? #(not= nil %)))
           extra (if-not form?
                   ; can we assert entity? No, bc we could model a link to a single relation without a form.
                   (if-let [entity (get relation "entity")] ; makes sense only for entity links, not query links as entity.
                     ^{:key (hash (keys entity))} [new-field entity param-ctx]))]

       (concat spec-fields [extra]))
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
