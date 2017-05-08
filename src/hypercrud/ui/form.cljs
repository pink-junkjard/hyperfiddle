(ns hypercrud.ui.form
  (:require [cursor.core :refer [cursor]]
            [hypercrud.types :refer [->DbVal]]
            [hypercrud.browser.connection-color :as connection-color]
            [hypercrud.ui.auto-control :refer [auto-control]]
            [hypercrud.ui.form-util :as form-util]
            [hypercrud.ui.input :as input]
            [hypercrud.ui.renderer :as renderer]
            [hypercrud.ui.widget :as widget]
            [reagent.core :as r]))

(defn field [maybe-field anchors param-ctx]
  [:div.field {:style {:border-color (connection-color/connection-color (:color param-ctx))}}
   [:label (form-util/field-label maybe-field param-ctx)]
   (let [anchors (filter #(= (-> param-ctx :attribute :db/id) (some-> % :anchor/attribute :db/id)) anchors)
         props (form-util/build-props maybe-field anchors param-ctx)]
     (if (renderer/user-renderer param-ctx)
       (renderer/user-render maybe-field anchors props param-ctx)
       [auto-control maybe-field anchors props param-ctx]))])

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

(defn form [relation colspec anchors param-ctx]
  [:div.forms-list
   ; all anchors need a find-element at least, because it has a connection affinity.
   (let [param-ctx (assoc param-ctx :result relation)
         entity-anchors-lookup (->> (remove :anchor/attribute anchors)
                                    (group-by (fn [anchor]
                                                ; link-entity's don't set find-element, but they get the entity in scope nonetheless.
                                                (if-let [find-element (:anchor/find-element anchor)]
                                                  (:find-element/name find-element)
                                                  "entity"))))
         fields (->> (partition 4 colspec)
                     (group-by second)
                     (mapcat
                       (fn [[fe-name colspec]]
                         (let [entity-anchors (get entity-anchors-lookup fe-name)]
                           (concat
                             ; This is counter intuitive. For link-entity-sys-new only, we need to conjure a connection,
                             ; so we put the entity in scope to do this. The value is not depended on because it's not truly
                             ; repeating. Another way to code it would be to just inspect all the result for a connection
                             ; and add the connection into the ctx. Because if we got here from a sys link, we know we have at least
                             ; something. What if we have a link-query with no results? then we know the connection from the
                             ; link-query. A link-entity with no results is not possible. There is no sys-link-blank.
                             #_(widget/render-anchors (mapv vector (remove :anchor/repeating? entity-anchors) (repeat param-ctx)))
                             (let [entity (get relation fe-name)
                                   param-ctx (form-util/entity-param-ctx entity param-ctx)]
                               #_(assert entity "i think this is true now")
                               (concat
                                 (widget/render-anchors (remove :anchor/render-inline? entity-anchors) param-ctx)
                                 (->> colspec
                                      (mapv (fn [[conn fe-name ident maybe-field]]
                                              (let [param-ctx (as-> param-ctx $
                                                                    ; :db/id is missing from schema so fake it here, it has no valueType
                                                                    (assoc $ :attribute (get (:schema param-ctx) ident {:attribute/ident ident})
                                                                             :value (get entity ident)
                                                                             :layout :form)
                                                                    (if (= ident :db/id) (assoc $ :read-only (constantly true)) $))]
                                                ^{:key (str ident)}
                                                [field maybe-field anchors param-ctx]))))
                                 (widget/render-inline-links (filter :anchor/render-inline? entity-anchors) param-ctx))))))))
         not-splat? (and (not (empty? colspec))
                         (->> (partition 4 colspec)
                              (mapv (fn [[conn fe-name attr maybe-field]] maybe-field))
                              (every? #(not= nil %))))
         magic-new-field (if-not not-splat?
                           ; can we assert entity? No, bc we could model a link to a single relation without a form.
                           (if-let [entity (get relation "entity")] ; makes sense only for entity links, not query links as entity.
                             ^{:key (hash (keys entity))} [new-field entity param-ctx]))]

     (concat fields [magic-new-field]))])
