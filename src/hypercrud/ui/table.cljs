(ns hypercrud.ui.table
  (:require [clojure.string :as string]
            [hypercrud.browser.connection-color :as connection-color]
            [hypercrud.runtime.state.actions :as actions]   ; todo bad dep
            [hypercrud.ui.auto-control :refer [auto-table-cell]]
            [hypercrud.ui.form-util :as form-util]
            [hypercrud.ui.renderer :as renderer]
            [hypercrud.ui.widget :as widget]
            [hypercrud.util.core :as util]
            [hypercrud.react.react-fragment :refer [react-fragment]]
            [reagent.core :as r]))


(defn sortable? [{:keys [:attribute/cardinality :attribute/valueType] :as attr}]
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

(defn build-col-heads [colspec anchors col-sort param-ctx]
  (->> (partition 4 colspec)
       (group-by (fn [[dbval fe attr maybe-field]] fe))
       (mapcat (fn [[fe colspec]]
                 (let [db (ffirst colspec)
                       param-ctx (assoc param-ctx :db db
                                                  :find-element fe
                                                  ; todo custom user-dispatch with all the tx-fns as reducers
                                                  :user-with! (fn [tx] ((:dispatch! param-ctx) (actions/with (.-conn-id db) (.-branch db) tx))))]
                   (->> colspec
                        (mapv (fn [[db fe attr field]]
                                (let [fe-name (-> fe :find-element/name)
                                      ident (-> attr :attribute/ident)
                                      param-ctx (assoc param-ctx :attribute attr)
                                      css-classes [(str "field-element-" (form-util/css-slugify fe-name))
                                                   (str "field-attr-" (form-util/css-slugify (str ident)))] #_"Dustin removed field-id and field-prompt; use a custom renderer"
                                      anchors (->> anchors
                                                   (filter #(= (-> attr :db/id) (-> % :anchor/attribute :db/id)))
                                                   #_(filter #(= (-> fe :db/id) (-> % :anchor/find-element :db/id))) #_"entity"
                                                   (remove :anchor/repeating?))
                                      [anchors] (widget/process-option-popover-anchors anchors param-ctx)

                                      [sort-fe-dbid sort-key direction] @col-sort
                                      with-sort-direction (fn [asc desc no-sort not-sortable]
                                                            (if (sortable? attr)
                                                              (if (and (= (:db/id fe) sort-fe-dbid) (= sort-key ident))
                                                                (case direction
                                                                  :asc asc
                                                                  :desc desc)
                                                                no-sort)
                                                              not-sortable))

                                      on-click (with-sort-direction #(reset! col-sort [(:db/id fe) ident :desc])
                                                                    #(reset! col-sort nil)
                                                                    #(reset! col-sort [(:db/id fe) ident :asc])
                                                                    (constantly nil))
                                      arrow (with-sort-direction " ↓" " ↑" " ↕" nil)]
                                  [:td {:class (string/join " " css-classes)
                                        :style {:background-color (connection-color/connection-color (or (:color param-ctx) (.-conn-id db) #_"hack for top tables"))}
                                        :key (str fe-name "-" ident)
                                        :on-click on-click}
                                   [:label (form-util/field-label field param-ctx)]
                                   [:div.anchors (widget/render-anchors (->> anchors (remove :anchor/render-inline?)) param-ctx)]
                                   (widget/render-inline-anchors (->> anchors (filter :anchor/render-inline?)) param-ctx)
                                   [:span.sort-arrow arrow]])))))))))

(defn control [maybe-field anchors param-ctx]
  (let [props (form-util/build-props maybe-field anchors param-ctx)]
    (if (renderer/user-renderer param-ctx)
      (renderer/user-render maybe-field anchors props param-ctx)
      [auto-table-cell maybe-field anchors props param-ctx])))

(defn field [control maybe-field anchors param-ctx]
  (let [shadow-link (not (-> param-ctx :entity :db/id))
        style {:border-color (if-not shadow-link (connection-color/connection-color (:color param-ctx)))}]
    [:td.truncate {:style style}
     (control param-ctx)]))

(defn Value [[db fe attr maybe-field] entity-anchors-lookup param-ctx]
  (let [fe-name (-> fe :find-element/name)
        ident (-> attr :attribute/ident)
        param-ctx (assoc param-ctx :attribute attr
                                   :value (get (:entity param-ctx) ident)
                                   :layout :table)
        field (case (:display-mode param-ctx) :xray field :user (get param-ctx :field field))
        control (case (:display-mode param-ctx) :xray control :user (get param-ctx :control control))

        anchors (filter #(= (-> param-ctx :attribute :db/id) (some-> % :anchor/attribute :db/id)) (get entity-anchors-lookup fe-name))]
    ^{:key (or (:db/id maybe-field) (str fe-name ident))}
    [field #(control maybe-field anchors %) maybe-field anchors param-ctx]))

(defn FindElement [[fe colspec] entity-anchors-lookup param-ctx]
  (let [entity (get (:result param-ctx) (-> fe :find-element/name))
        db (ffirst colspec)
        param-ctx (-> param-ctx
                      (assoc :db db
                             :find-element fe
                             ; todo custom user-dispatch with all the tx-fns as reducers
                             :user-with! (fn [tx] ((:dispatch! param-ctx) (actions/with (.-conn-id db) (.-branch db) tx))))
                      (form-util/entity-param-ctx entity))]
    (->> colspec (mapv #(Value % entity-anchors-lookup param-ctx)))))

(defn Relation [relation colspec fe-anchors-lookup param-ctx]
  (->> (partition 4 colspec)
       (group-by (fn [[db fe ident maybe-field]] fe))
       (mapcat #(FindElement % fe-anchors-lookup param-ctx))))

(defn Row [relation colspec anchors param-ctx]
  (let [param-ctx (assoc param-ctx :result relation) ; todo :result -> :relation
        fe-anchors-lookup (group-by (comp :find-element/name :anchor/find-element) anchors)]
    ^{:key (hash (util/map-values #(or (:db/id %) (-> % :anchor/ident)) relation))}
    [:tr
     (apply react-fragment :table-row-form (Relation relation colspec fe-anchors-lookup param-ctx))
     [:td.link-cell #_{:key :link-cell}
      ; inline entity-anchors are not yet implemented, what does that mean.
      (widget/render-anchors (->> (partition 4 colspec)
                                  (group-by (fn [[db fe attr maybe-field]] fe)) ; keys are set, ignore colspec now except for db which is uniform.
                                  (mapcat (fn [[fe colspec]]
                                            (let [fe-name (-> fe :find-element/name)
                                                  entity (get relation fe-name) _ (assert entity)
                                                  param-ctx (-> param-ctx
                                                                (assoc :db (ffirst colspec) :find-element fe)
                                                                (form-util/entity-param-ctx entity))
                                                  fe-anchors (->> (get fe-anchors-lookup fe-name)
                                                                  (filter :anchor/repeating?)
                                                                  (remove :anchor/attribute)
                                                                  (remove :anchor/render-inline?))]
                                              (mapv vector fe-anchors (repeat param-ctx)))))))]]))

(defn Resultset [relations colspec anchors sort-col param-ctx]
  (let [[fe-dbid sort-key direction] @sort-col
        sort-fn (fn [relations]
                    (let [[_ fe attr _] (->> (partition 4 colspec)
                                             (filter (fn [[db fe attr maybe-field]]
                                                       (and (= fe-dbid (:db/id fe))
                                                            (= (:attribute/ident attr) sort-key))))
                                             first)]
                      (if (sortable? attr)
                        (sort-by #(get-in % [(:find-element/name fe) sort-key])
                                 (case direction
                                   :asc #(compare %1 %2)
                                   :desc #(compare %2 %1))
                                 relations)
                        relations)))]
    (->> relations sort-fn (map #(Row % colspec anchors param-ctx)))))

(defn table [& props]
  (let [sort-col (r/atom nil)]
    (fn [relations colspec anchors param-ctx]
      (let [anchors (widget/process-popover-anchors anchors param-ctx)

            links-index (widget/render-anchors (->> anchors
                                                    (remove :anchor/attribute)
                                                    (remove :anchor/find-element)
                                                    (remove :anchor/render-inline?))
                                               (dissoc param-ctx :isComponent))
            links-fe-no-entity (let [anchors-lookup (->> anchors
                                                         (remove :anchor/repeating?)
                                                         (remove :anchor/attribute)
                                                         (remove :anchor/render-inline?)
                                                         (group-by (comp :find-element/name :anchor/find-element)))]
                                 (->> (partition 4 colspec)
                                      (group-by (fn [[dbval fe attr maybe-field]] fe))
                                      (mapcat (fn [[fe colspec]]
                                                (let [form-anchors (get anchors-lookup (-> fe :find-element/name))
                                                      param-ctx (assoc param-ctx :db (ffirst colspec)
                                                                                 :find-element fe)]
                                                  (widget/render-anchors form-anchors param-ctx))))))
            links-index-inline (widget/render-inline-anchors (->> anchors ; busted
                                                                  (remove :anchor/repeating?)
                                                                  (remove :anchor/attribute)
                                                                  (filter :anchor/render-inline?))
                                                             (dissoc param-ctx :isComponent))]
        [:div.ui-table-with-links
         links-index
         [:table.ui-table
          [:thead [:tr
                   (build-col-heads colspec anchors sort-col param-ctx)
                   [:td.link-cell {:key :link-cell} links-fe-no-entity]]]
          ; Sometimes the leafnode needs all the anchors.
          [:tbody (apply react-fragment :tbody (Resultset relations colspec anchors sort-col param-ctx))]]
         links-index-inline]))))
