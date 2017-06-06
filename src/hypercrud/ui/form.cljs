(ns hypercrud.ui.form
  (:require [hypercrud.browser.connection-color :as connection-color]
            [hypercrud.ui.auto-control :refer [auto-control]]
            [hypercrud.ui.form-util :as form-util]
            [hypercrud.ui.input :as input]
            [hypercrud.ui.renderer :as renderer]
            [hypercrud.ui.tooltip :as tooltip]
            [hypercrud.ui.widget :as widget]
            [hypercrud.util.core :as util]
            [reagent.core :as r]))

(defn field [maybe-field anchors param-ctx]
  (let [control (let [anchors (filter #(= (-> param-ctx :attribute :db/id) (some-> % :anchor/attribute :db/id)) anchors)
                      props (form-util/build-props maybe-field anchors param-ctx)]
                  (if (renderer/user-renderer param-ctx)
                    (renderer/user-render maybe-field anchors props param-ctx)
                    [auto-control maybe-field anchors props param-ctx]))]

    [:div.field {:style {:border-color (connection-color/connection-color (:color param-ctx))}}
     (case (:layout param-ctx)
       :block [:label (form-util/field-label maybe-field param-ctx)]
       :inline-block nil)
     (case (:layout param-ctx)
       :block control
       :inline-block
       [tooltip/hover-tooltip-managed {:label (util/fallback empty? (get maybe-field :field/prompt) (-> param-ctx :attribute :attribute/ident str))}
        control])]))

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

(def always-read-only (constantly true))

(defn form [relation colspec anchors param-ctx]
  ; all anchors need a find-element at least, because it has a connection affinity.
  (let [param-ctx (assoc param-ctx :result relation
                                   :layout (:layout param-ctx :block))
        anchors (widget/process-popover-anchors anchors param-ctx)
        anchors-lookup (->> (remove :anchor/attribute anchors)
                            (group-by (fn [anchor]
                                        (if-let [find-element (:anchor/find-element anchor)]
                                          (:find-element/name find-element)
                                          "entity"))))
        fields (->> (partition 4 colspec)
                    (group-by (fn [[dbval fe-name ident maybe-field]] fe-name))
                    (mapcat
                      (fn [[fe-name colspec]]
                        (let [form-anchors (get anchors-lookup fe-name)
                              entity-new-anchors (->> form-anchors (remove :anchor/repeating?))
                              entity-anchors (->> form-anchors (filter :anchor/repeating?))
                              db (ffirst colspec)
                              param-ctx (assoc param-ctx :db db
                                                         :user-swap! (partial (:app-swap! param-ctx) (.-conn-id db) (.-branch db)))]
                          (concat
                            ; don't put entity in scope because it messes up formulas which have to be deterministic with request side.
                            (widget/render-anchors (remove :anchor/render-inline? entity-new-anchors) param-ctx)
                            (let [entity (get relation fe-name)
                                  param-ctx (form-util/entity-param-ctx entity param-ctx)]
                              #_(assert entity "i think this is true now")
                              (concat
                                (widget/render-anchors (remove :anchor/render-inline? entity-anchors) param-ctx)
                                (->> colspec
                                     (mapv (fn [[db fe-name ident maybe-field]]
                                             (let [param-ctx (as-> param-ctx $
                                                                   ; :db/id is missing from schema so fake it here, it has no valueType
                                                                   (assoc $ :attribute (get (:schema param-ctx) ident {:attribute/ident ident})
                                                                            :value (get entity ident))
                                                                   (if (= ident :db/id) (assoc $ :read-only always-read-only) $))]
                                               ^{:key (str ident)}
                                               [field maybe-field anchors param-ctx]))))
                                (widget/render-inline-anchors (filter :anchor/render-inline? entity-anchors) param-ctx)))
                            (widget/render-inline-anchors (filter :anchor/render-inline? entity-new-anchors) param-ctx))))))
        not-splat? (and (not (empty? colspec))
                        (->> (partition 4 colspec)
                             (mapv (fn [[conn fe-name attr maybe-field]] maybe-field))
                             (every? #(not= nil %))))
        magic-new-field (if-not not-splat?
                          ; can we assert entity? No, bc we could model a link to a single relation without a form.
                          (if-let [entity (get relation "entity")] ; makes sense only for entity links, not query links as entity.
                            ^{:key (hash (keys entity))} [new-field entity param-ctx]))]

    [:div {:class (str "forms-list " (name (:layout param-ctx)))}
     (concat fields [magic-new-field])])
  ; Can't differentiate between index links and entity links on link-entity right now.
  #_(widget/render-inline-anchors (->> anchors
                                       (remove :anchor/repeating?)
                                       (remove :anchor/attribute)
                                       (filter :anchor/render-inline?))
                                  (dissoc param-ctx :isComponent))
  )
