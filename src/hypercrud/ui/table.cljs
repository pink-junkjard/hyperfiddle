(ns hypercrud.ui.table
  (:require [clojure.string :as string]
            [hypercrud.platform.native-event-listener :refer [native-listener]] ;provided dependency
            [hypercrud.ui.auto-control :refer [auto-table-cell]]
            [hypercrud.ui.renderer :as renderer]
            [hypercrud.ui.widget :as widget]
            [hypercrud.browser.connection-color :as connection-color]
            [hypercrud.util :as util]
            [reagent.core :as r]
            [hypercrud.ui.form-util :as form-util]))


(defn sortable? [field]
  (let [{:keys [:attribute/cardinality :attribute/valueType]} (:field/attribute field)]
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

(defn build-col-heads [colspec col-sort param-ctx]
  (->> (partition 4 colspec)
       (group-by (fn [[dbval fe-name ident maybe-field]] fe-name))
       (mapcat (fn [[fe-name colspec]]
                 (let [db (ffirst colspec)
                       param-ctx (assoc param-ctx :db db
                                                  :user-swap! (partial (:user-swap! param-ctx) (.-conn-id db) (.-branch db)))]
                   (->> colspec
                        (mapv (fn [[db fe-name ident field]]
                                (let [param-ctx (assoc param-ctx :attribute (get (:schema param-ctx) ident {:attribute/ident ident}))
                                      css-classes [(str "field-element-" (form-util/css-slugify fe-name))
                                                   (str "field-attr-" (form-util/css-slugify (str ident)))] #_"Dustin removed field-id and field-prompt; use a custom renderer"
                                      on-click #()

                                      ;with-sort-direction (fn [asc desc no-sort not-sortable]
                                      ;                      (if (sortable? field)
                                      ;                        (if (and (= form-dbid' form-dbid) (= ident' ident))
                                      ;                          (case direction
                                      ;                            :asc asc
                                      ;                            :desc desc)
                                      ;                          no-sort)
                                      ;                        not-sortable))

                                      ;on-click (with-sort-direction #(reset! col-sort [form-dbid ident :desc])
                                      ;                              #(reset! col-sort nil)
                                      ;                              #(reset! col-sort [form-dbid ident :asc])
                                      ;                              (constantly nil))
                                      ;arrow (with-sort-direction " ↓" " ↑" " ↕" nil)

                                      ]

                                  [:td {:class (string/join " " css-classes)
                                        :style {:background-color (connection-color/connection-color (or (:color param-ctx)
                                                                                                         ; hack for top tables
                                                                                                         (.-conn-id db)))}
                                        :key (str fe-name "-" ident)
                                        :on-click on-click}
                                   [:label (form-util/field-label field param-ctx)]
                                   #_[:span.sort-arrow arrow]])))))))))

(defn table-row-form [relation colspec anchors param-ctx]
  (let [entity-anchors-lookup (->> anchors
                                   ; entity anchors are guaranteed repeating
                                   ; attr anchors might be not-repeating for create-new
                                   (group-by (fn [anchor]
                                               (if-let [find-element (:anchor/find-element anchor)]
                                                 (:find-element/name find-element)
                                                 "entity"))))]
    [:tr
     (->> (partition 4 colspec)
          (group-by (fn [[db fe-name ident maybe-field]] fe-name))
          (mapcat (fn [[fe-name colspec]]
                    (let [entity (get relation fe-name)
                          db (ffirst colspec)
                          param-ctx (assoc param-ctx :db db
                                                     :user-swap! (partial (:user-swap! param-ctx) (.-conn-id db) (.-branch db)))
                          param-ctx (form-util/entity-param-ctx entity param-ctx)
                          attribute-anchors (->> (get entity-anchors-lookup fe-name)
                                                 (filter :anchor/attribute))]
                      (->> colspec
                           (mapv (fn [[db fe-name ident maybe-field]]
                                   (let [param-ctx (assoc param-ctx :attribute (get (:schema param-ctx) ident {:attribute/ident ident})
                                                                    :value (get entity ident)
                                                                    :layout :table)
                                         style {:border-color (connection-color/connection-color (:color param-ctx))}]
                                     [:td.truncate {:key (or (:db/id maybe-field) (str fe-name ident)) :style style}
                                      (let [anchors (filter #(= (-> param-ctx :attribute :db/id)
                                                                (some-> % :anchor/attribute :db/id)) attribute-anchors)
                                            props (form-util/build-props maybe-field anchors param-ctx)]
                                        (if (renderer/user-renderer param-ctx)
                                          (renderer/user-render maybe-field anchors props param-ctx)
                                          [auto-table-cell maybe-field anchors props param-ctx]))]))))))))

     [:td.link-cell {:key :link-cell}
      ; inline entity-anchors are not yet implemented, what does that mean.
      ; find-element anchors need more items in their ctx
      (widget/render-anchors (->> (partition 4 colspec)
                                  (group-by (fn [[db fe-name ident maybe-field]] fe-name)) ; keys are set, ignore colspec now except for db which is uniform.
                                  (mapcat (fn [[fe-name colspec]]
                                            (let [entity (get relation fe-name) _ (assert entity)
                                                  param-ctx (assoc param-ctx :db (ffirst colspec))
                                                  param-ctx (form-util/entity-param-ctx entity param-ctx)
                                                  fe-anchors (->> (get entity-anchors-lookup fe-name)
                                                                  (filter :anchor/repeating?)
                                                                  (remove :anchor/attribute)
                                                                  (remove :anchor/render-inline?))]
                                              (mapv vector fe-anchors (repeat param-ctx)))))))]]))


(defn body [relations colspec anchors sort-col param-ctx]
  [:tbody
   (let [[form-dbid sort-key direction] @sort-col
         ;sort-eids (fn [resultset]
         ;            (let [{form :find-element/form find-element-name :find-element/name}
         ;                  (->> ordered-find-elements
         ;                       (filter #(= form-dbid (-> % :find-element/form :db/id)))
         ;                       first)
         ;                  field (->> (:form/field form)
         ;                             (filter #(= sort-key (-> % :field/attribute :attribute/ident)))
         ;                             first)]
         ;              (if (and (not= nil field) (sortable? field))
         ;                (sort-by #(get-in % [find-element-name sort-key])
         ;                         (case direction
         ;                           :asc #(compare %1 %2)
         ;                           :desc #(compare %2 %1))
         ;                         resultset)
         ;                resultset)))
         ]
     (->> relations
          #_sort-eids
          (map (fn [relation]
                 (let [param-ctx (assoc param-ctx :result relation)] ; todo :result -> :relation
                   ^{:key (hash (util/map-values :db/id relation))}
                   [table-row-form relation colspec anchors param-ctx])))))])

(defn table [& props]
  (let [sort-col (r/atom nil)]
    (fn [relations colspec anchors param-ctx]
      (let [anchors (widget/process-popover-anchors anchors param-ctx)]
        [:div.ui-table-with-links
         [:table.ui-table
          [:thead
           [:tr
            (build-col-heads colspec sort-col param-ctx)
            [:td.link-cell {:key :link-cell}
             (let [anchors-lookup (->> anchors
                                       (remove :anchor/repeating?)
                                       (remove :anchor/attribute)
                                       (remove :anchor/render-inline?)
                                       (group-by (fn [anchor]
                                                   (if-let [find-element (:anchor/find-element anchor)]
                                                     (:find-element/name find-element)
                                                     "entity"))))]
               (->> (partition 4 colspec)
                    (group-by (fn [[dbval fe-name ident maybe-field]] fe-name))
                    (mapcat (fn [[fe-name colspec]]
                              (let [form-anchors (get anchors-lookup fe-name)
                                    param-ctx (assoc param-ctx :db (ffirst colspec))]
                                (widget/render-anchors form-anchors param-ctx))))))]]]
          ; filter repeating? No because create-new-attribute down here too.
          [body relations colspec anchors sort-col param-ctx]]
         (widget/render-inline-anchors (->> anchors         ; busted
                                            (remove :anchor/repeating?)
                                            (remove :anchor/attribute)
                                            (filter :anchor/render-inline?))
                                       (dissoc param-ctx :isComponent))]))))
