(ns hypercrud.ui.table
  (:require [clojure.string :as string]
            [hypercrud.browser.anchor :as anchor]
            [hypercrud.browser.auto-anchor :as auto-anchor]
            [hypercrud.browser.connection-color :as connection-color]
            [hypercrud.browser.context :as context]
            [hypercrud.ui.auto-control :refer [auto-table-cell]]
            [hypercrud.ui.form-util :as form-util]
            [hypercrud.ui.renderer :as renderer]
            [hypercrud.ui.widget :as widget]
            [hypercrud.util.core :as util]
            [reagent.core :as r]))


(defn attr-sortable? [fe attribute ctx]
  (let [{:keys [:db/cardinality :db/valueType]} (get-in ctx [:schemas (:find-element/name fe) attribute])]
    (and
      (= (:db/ident cardinality) :db.cardinality/one)
      ; ref requires more work (inspect label-prop)
      (contains? #{:db.type/keyword
                   :db.type/string
                   :db.type/boolean
                   :db.type/long
                   :db.type/bigint
                   :db.type/float
                   :db.type/double
                   :db.type/bigdec
                   :db.type/instant
                   :db.type/uuid
                   :db.type/uri
                   :db.type/bytes
                   :db.type/code}
                 (:db/ident valueType)))))

(defn col-head-anchors [attr-label-anchors ctx]
  [:div.anchors
   (widget/render-anchors (remove :anchor/render-inline? attr-label-anchors) ctx)
   (widget/render-inline-anchors (filter :anchor/render-inline? attr-label-anchors) ctx)])

(defn col-head [fe {:keys [:field/attribute] :as field} anchors-lookup sort-col ctx]
  (let [fe-name (:find-element/name fe)
        ctx (context/attribute ctx attribute)
        [attr-label-anchors] (-> (->> (get-in anchors-lookup [fe-name attribute])
                                      (remove :anchor/repeating?))
                                 (widget/process-option-popover-anchors ctx))
        sortable? (and (not-any? anchor/popover-anchor? attr-label-anchors) ; sorting currently breaks click handling in popovers
                       (attr-sortable? fe attribute ctx))
        sort-direction (let [[sort-fe-dbid sort-key direction] @sort-col]
                         (if (and (= (:db/id fe) sort-fe-dbid) (= sort-key attribute))
                           direction))
        on-click (fn []
                   (if sortable?
                     (reset! sort-col (case sort-direction
                                        :asc [(:db/id fe) attribute :desc]
                                        :desc nil
                                        [(:db/id fe) attribute :asc]))))
        css-classes [(str "field-element-" (form-util/css-slugify fe-name))
                     (str "field-attr-" (form-util/css-slugify (str attribute))) #_"Dustin removed field-id and field-prompt; use a custom renderer"
                     (if sortable? "sortable")
                     (some-> sort-direction name)]]
    [:th {:class (string/join " " css-classes)
          :style {:background-color (connection-color/connection-color (:uri ctx) ctx)}
          :on-click on-click}
     [:label [form-util/field-label field ctx]]
     [col-head-anchors attr-label-anchors ctx]]))

(defn LinkCell [repeating? ordered-fes anchors-lookup ctx]
  [(if repeating? :td.link-cell :th.link-cell)
   (->> ordered-fes
        (mapcat (fn [fe]
                  (let [fe-name (:find-element/name fe)
                        ctx (as-> (context/find-element ctx fe) ctx
                                  (if repeating?
                                    (context/entity ctx (get-in ctx [:relation fe-name]))
                                    ctx))
                        form-anchors (->> (get-in anchors-lookup [fe-name nil])
                                          ((if repeating? filter remove) :anchor/repeating?)
                                          ; inline entity-anchors are not yet implemented, what does that mean.
                                          (remove :anchor/render-inline?))]
                    (widget/render-anchors form-anchors ctx)))))])

(defn HeaderRow [ordered-fes anchors-lookup sort-col ctx]
  [:tr
   (->> ordered-fes
        (mapcat (fn [{:keys [] :as fe}]
                  (let [ctx (context/find-element ctx fe)]
                    (->> (-> fe :find-element/form :form/field)
                         (map (fn [field]
                                ^{:key (str (:find-element/name fe) "-" (:field/attribute field))}
                                [col-head fe field anchors-lookup sort-col ctx])))))))
   [LinkCell false ordered-fes anchors-lookup ctx]])

(defn Control [field anchors ctx]
  (let [props (form-util/build-props field anchors ctx)]
    (if (renderer/user-renderer ctx)
      (renderer/user-render field anchors props ctx)
      [auto-table-cell field anchors props ctx])))

(defn Field [control field anchors ctx]
  (let [shadow-link (auto-anchor/system-anchor? (-> ctx :entity :db/id))
        style {:border-color (if-not shadow-link (connection-color/connection-color (:uri ctx) ctx))}]
    [:td.truncate {:style style}
     [control ctx]]))

(defn Value [{:keys [:field/attribute] :as field} fe-anchors-lookup ctx]
  (let [ctx (-> (context/attribute ctx attribute)
                (context/value (get (:entity ctx) attribute)) ; Not reactive
                (assoc :layout :table))
        display-mode @(:display-mode ctx)
        Field (case display-mode :xray Field :user (get ctx :field Field))
        Control (case display-mode :xray Control :user (get ctx :control Control))
        attr-anchors (get fe-anchors-lookup attribute)]
    [Field (r/partial Control field attr-anchors) field attr-anchors ctx]))

(defn FindElement [fe anchors-lookup ctx]
  (let [fe-name (:find-element/name fe)
        entity (get (:relation ctx) fe-name)
        fe-anchors-lookup (get anchors-lookup fe-name)
        ctx (-> (context/find-element ctx fe)
                (context/entity entity))]
    (->> (-> fe :find-element/form :form/field)
         (mapv (fn [field]
                 ; todo this could clash if you use the same form on two findelements
                 ^{:key (:db/id field)}
                 [Value field fe-anchors-lookup ctx])))))

(defn Relation [relation ordered-fes anchors-lookup ctx]
  (mapcat #(FindElement % anchors-lookup ctx) ordered-fes))

(defn Row [relation ordered-fes anchors-lookup ctx]
  (let [ctx (context/relation ctx relation)]
    [:tr
     (Relation relation ordered-fes anchors-lookup ctx)
     (LinkCell true ordered-fes anchors-lookup ctx)]))

(defn Resultset [relations ordered-fes anchors-lookup sort-col ctx]
  (let [[fe-dbid sort-key direction] @sort-col
        sort-fn (fn [relations]
                  (let [fe (->> ordered-fes
                                (filter (fn [fe] (= fe-dbid (:db/id fe))))
                                first)
                        attr (->> (-> fe :find-element/form :form/field)
                                  (map :field/attribute)
                                  (filter #(= % sort-key))
                                  first)]
                    (if (attr-sortable? fe attr ctx)
                      (sort-by #(get-in % [(:find-element/name fe) sort-key])
                               (case direction
                                 :asc #(compare %1 %2)
                                 :desc #(compare %2 %1))
                               relations)
                      relations)))]
    (->> relations
         sort-fn
         (map (fn [relation]
                ^{:key (hash (util/map-values :db/id relation))}
                [Row relation ordered-fes anchors-lookup ctx])))))

(defn Table [& props]
  (let [sort-col (r/atom nil)]
    (fn [relations ordered-fes anchors-lookup ctx]
      [:table.ui-table
       [:thead [HeaderRow ordered-fes anchors-lookup sort-col ctx]]
       ; Sometimes the leafnode needs all the anchors.
       [:tbody (Resultset relations ordered-fes anchors-lookup sort-col ctx)]])))

(defn ui-table [relations ordered-fes anchors ctx]
  (let [anchors-lookup (->> (widget/process-popover-anchors anchors ctx)
                            (group-by (comp :find-element/name :anchor/find-element))
                            (util/map-values (partial group-by :anchor/attribute)))
        {inline-index-anchors true index-anchors false} (->> (get-in anchors-lookup [nil nil])
                                                             (group-by :anchor/render-inline?))
        index-ctx (dissoc ctx :isComponent)]
    [:div
     (widget/render-anchors index-anchors index-ctx)
     (if (every? #(empty? (-> % :find-element/form :form/field)) ordered-fes)
       [:div "Can't infer table structure - no resultset and blank form. Fix query or model a form."]
       [Table relations ordered-fes anchors-lookup ctx])
     (widget/render-inline-anchors inline-index-anchors index-ctx)]))
