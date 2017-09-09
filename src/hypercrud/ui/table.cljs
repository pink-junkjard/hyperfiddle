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
            [hypercrud.react.react-fragment :refer [react-fragment]]
            [reagent.core :as r]))


(defn attr-sortable? [{:keys [:db/cardinality :db/valueType] :as attr}]
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
               (:db/ident valueType))))

(defn col-head-anchors [attr-label-anchors ctx]
  [:div.anchors
   (widget/render-anchors (remove :anchor/render-inline? attr-label-anchors) ctx)
   (widget/render-inline-anchors (filter :anchor/render-inline? attr-label-anchors) ctx)])

(defn col-head [[db fe {:keys [:db/ident] :as attr} field] anchors-lookup sort-col ctx]
  (let [fe-name (:find-element/name fe)
        ctx (context/attribute ctx attr)
        [attr-label-anchors] (-> (->> (get-in anchors-lookup [fe-name ident])
                                      (remove :anchor/repeating?))
                                 (widget/process-option-popover-anchors ctx))
        sortable? (and (not-any? anchor/popover-anchor? attr-label-anchors) ; sorting currently breaks click handling in popovers
                       (attr-sortable? attr))
        sort-direction (let [[sort-fe-dbid sort-key direction] @sort-col]
                         (if (and (= (:db/id fe) sort-fe-dbid) (= sort-key ident))
                           direction))
        on-click (fn []
                   (if sortable?
                     (reset! sort-col (case sort-direction
                                        :asc [(:db/id fe) ident :desc]
                                        :desc nil
                                        [(:db/id fe) ident :asc]))))
        css-classes [(str "field-element-" (form-util/css-slugify fe-name))
                     (str "field-attr-" (form-util/css-slugify (str ident))) #_"Dustin removed field-id and field-prompt; use a custom renderer"
                     (if sortable? "sortable")
                     (some-> sort-direction name)]]
    [:th {:class (string/join " " css-classes)
          :style {:background-color (connection-color/connection-color (or (:color ctx) (.-conn-id db) #_"hack for top tables"))}
          :on-click on-click}
     [:label (form-util/field-label field ctx)]
     [col-head-anchors attr-label-anchors ctx]]))

(defn LinkCell [repeating? colspec anchors-lookup ctx]
  [(if repeating? :td.link-cell :th.link-cell)
   (->> (partition 4 colspec)
        (group-by (fn [[dbval fe attr maybe-field]] fe))
        (mapcat (fn [[fe colspec]]
                  (let [fe-name (:find-element/name fe)
                        ctx (as-> (context/find-element ctx fe) ctx
                                  (if repeating?
                                    (context/entity ctx (get-in ctx [:result fe-name]))
                                    ctx))
                        form-anchors (->> (get-in anchors-lookup [fe-name nil])
                                          ((if repeating? filter remove) :anchor/repeating?)
                                          ; inline entity-anchors are not yet implemented, what does that mean.
                                          (remove :anchor/render-inline?))]
                    (widget/render-anchors form-anchors ctx)))))])

; basically the same type signature and impl as Row
(defn HeaderRow [colspec anchors-lookup sort-col ctx]
  [:tr
   (->> (partition 4 colspec)
        (group-by (fn [[dbval fe attr maybe-field]] fe))
        (mapcat (fn [[fe colspec]]
                  (let [param-ctx (context/find-element ctx fe)]
                    (->> colspec
                         (map (fn [[_ fe attr _ :as col]]
                                ^{:key (str (:find-element/name fe) "-" (:db/ident attr))}
                                [col-head col anchors-lookup sort-col param-ctx])))))))
   [LinkCell false colspec anchors-lookup ctx]])

(defn Control [maybe-field anchors param-ctx]
  (let [props (form-util/build-props maybe-field anchors param-ctx)]
    (if (renderer/user-renderer param-ctx)
      (renderer/user-render maybe-field anchors props param-ctx)
      [auto-table-cell maybe-field anchors props param-ctx])))

(defn Field [control maybe-field anchors param-ctx]
  (let [shadow-link (auto-anchor/system-anchor? (-> param-ctx :entity :db/id))
        style {:border-color (if-not shadow-link (connection-color/connection-color (:color param-ctx)))}]
    [:td.truncate {:style style}
     (control param-ctx)]))

(defn Value [[db fe attr maybe-field] entity-anchors-lookup param-ctx]
  (let [ident (-> attr :db/ident)
        param-ctx (-> (context/attribute param-ctx attr)
                      (context/value (get (:entity param-ctx) ident))
                      (assoc :layout :table))
        field (case (:display-mode param-ctx) :xray Field :user (get param-ctx :field Field))
        control (case (:display-mode param-ctx) :xray Control :user (get param-ctx :control Control))
        attr-anchors (get entity-anchors-lookup ident)]
    [field #(control maybe-field attr-anchors %) maybe-field attr-anchors param-ctx]))

(defn FindElement [[fe colspec] anchors-lookup param-ctx]
  (let [entity (get (:result param-ctx) (-> fe :find-element/name))
        param-ctx (-> (context/find-element param-ctx fe)
                      (context/entity entity))]
    (->> colspec
         (mapv (fn [[_ fe attr maybe-field :as col]]
                 (let [entity-anchors-lookup (get anchors-lookup (:find-element/name fe))]
                   ^{:key (or (:db/id maybe-field) (str (:find-element/name fe) (:db/ident attr)))}
                   [Value col entity-anchors-lookup param-ctx]))))))

(defn Relation [relation colspec anchors-lookup param-ctx]
  (->> (partition 4 colspec)
       (group-by (fn [[db fe ident maybe-field]] fe))
       (mapcat #(FindElement % anchors-lookup param-ctx))))

(defn Row [relation colspec anchors-lookup param-ctx]
  (let [param-ctx (context/relation param-ctx relation)]
    [:tr
     (apply react-fragment :table-row-form (Relation relation colspec anchors-lookup param-ctx))
     (LinkCell true colspec anchors-lookup param-ctx)]))

(defn Resultset [relations colspec anchors-lookup sort-col param-ctx]
  (let [[fe-dbid sort-key direction] @sort-col
        sort-fn (fn [relations]
                  (let [[_ fe attr _] (->> (partition 4 colspec)
                                           (filter (fn [[db fe attr maybe-field]]
                                                     (and (= fe-dbid (:db/id fe))
                                                          (= (:db/ident attr) sort-key))))
                                           first)]
                    (if (attr-sortable? attr)
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
                [Row relation colspec anchors-lookup param-ctx])))))

(defn Table [& props]
  (let [sort-col (r/atom nil)]
    (fn [relations colspec anchors-lookup ctx]
      [:table.ui-table
       [:thead [HeaderRow colspec anchors-lookup sort-col ctx]]
       ; Sometimes the leafnode needs all the anchors.
       [:tbody (apply react-fragment :tbody (Resultset relations colspec anchors-lookup sort-col ctx))]])))

(defn ui-table [relations colspec anchors ctx]
  (let [anchors-lookup (->> (widget/process-popover-anchors anchors ctx)
                            (group-by (comp :find-element/name :anchor/find-element))
                            (util/map-values (partial group-by :anchor/attribute)))

        links-index (widget/render-anchors (->> (get-in anchors-lookup [nil nil])
                                                (remove :anchor/render-inline?))
                                           (dissoc ctx :isComponent))
        links-index-inline (widget/render-inline-anchors (->> (get-in anchors-lookup [nil nil])
                                                              (filter :anchor/render-inline?))
                                                         (dissoc ctx :isComponent))]
    [:div.ui-table-with-links
     links-index
     (if (empty? colspec)
       [:div "Can't infer table structure - no resultset and blank form. Fix query or model a form."]
       [Table relations colspec anchors-lookup ctx])
     links-index-inline]))
