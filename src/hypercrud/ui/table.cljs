(ns hypercrud.ui.table
  (:require [clojure.string :as string]
            [hypercrud.browser.links :as links]
            [hypercrud.compile.eval :refer [eval]]
            [hypercrud.platform.native-event-listener :refer [native-listener]] ;provided dependency
            [hypercrud.types :refer [->DbId ->DbVal]]
            [hypercrud.ui.auto-control :refer [auto-table-cell raw-table-cell connection-color]]
            [hypercrud.ui.renderer :as renderer]
            [hypercrud.ui.widget :as widget]
            [hypercrud.ui.form-util :as form-util]
            [hypercrud.util :as util]
            [reagent.core :as r]))


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


(defn css-slugify [s]
  ; http://stackoverflow.com/a/449000/959627
  (-> s
      (string/replace ":" "-")
      (string/replace "/" "-")
      (string/replace " " "-")))


(defn build-col-heads [colspec col-sort]
  (->> (partition 3 colspec)
       (mapv (fn [[fe-name ident field]]
               (let [prompt (get field :field/prompt (str ident))
                     css-classes [(str "field-element-" (css-slugify fe-name))
                                  (str "field-attr-" (css-slugify (str ident)))] #_"Dustin removed field-id and field-prompt; use a custom renderer"
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

                 [:td {:class (string/join " " css-classes) :key (str fe-name "-" ident) :on-click on-click}
                  (str prompt)
                  (let [docstring (-> field :field/attribute :attribute/doc)]
                    (if-not (empty? docstring)
                      [native-listener {:on-click (fn [e]
                                                    (js/alert docstring)
                                                    (.stopPropagation e))}
                       [:span.help "ⓘ"]]))
                  #_[:span.sort-arrow arrow]])))
       (seq)))

(defn entity-param-ctx [entity param-ctx]
  (assoc param-ctx :color ((:color-fn param-ctx) entity param-ctx)
                   :owner ((:owner-fn param-ctx) entity param-ctx)
                   :entity entity))

(defn table-row-form [relation colspec repeating-anchors param-ctx]
  (let [find-element-anchors-lookup (->> repeating-anchors
                                         ; entity links can have attributes but not find-elements specified
                                         (filter #(or (:anchor/find-element %) (:anchor/attribute %)))
                                         (group-by (fn [anchor]
                                                     (if-let [find-element (:anchor/find-element anchor)]
                                                       (:find-element/name find-element)
                                                       "entity"))))]
    [:tr
     (->> (partition 3 colspec)
          (mapv (fn [[fe-name ident maybe-field]]           ; (fe-name, ident) are unique if taken together
                  (let [entity (get relation fe-name)
                        param-ctx (entity-param-ctx entity param-ctx)
                        param-ctx (assoc param-ctx :attribute (get (:schema param-ctx) ident))

                        ; rebuilt too much due to joining fe-name X ident
                        attribute-anchors (->> (get find-element-anchors-lookup fe-name)
                                               (remove #(nil? (:anchor/attribute %))))


                        style {:border-color (connection-color (:color param-ctx))}
                        value (get entity ident)]
                    [:td.truncate {:key (or (:db/id maybe-field) (str fe-name ident)) :style style}
                     (if-let [renderer (renderer/renderer-for-attribute (:field/attribute maybe-field))]
                       (let [{renderer :value error :error} (eval renderer)]
                         [:div.value
                          (if error
                            (pr-str error)
                            (try
                              (let [link-fn (let [anchor-by-ident (->> attribute-anchors
                                                                       (mapv (juxt #(-> % :anchor/ident) identity))
                                                                       (into {}))]
                                              (fn [ident label param-ctx]
                                                (let [anchor (get anchor-by-ident ident)
                                                      props (links/build-link-props anchor param-ctx)]
                                                  [(:navigate-cmp param-ctx) props label param-ctx])))]
                                (renderer (:peer param-ctx) link-fn value))
                              (catch :default e (pr-str e))))])
                       (let [anchors (filter #(= (-> param-ctx :attribute :db/id) (some-> % :anchor/attribute :db/id)) attribute-anchors)]
                         [auto-table-cell value maybe-field anchors param-ctx]))])))
          (seq))

     [:td.link-cell {:key :link-cell}
      ; render all repeating links (regardless if inline) as anchors
      (widget/render-anchors (concat
                               (mapv vector
                                     (->> repeating-anchors
                                          (filter #(nil? (:anchor/find-element %)))
                                          (filter #(nil? (:anchor/attribute %))))
                                     (repeatedly (constantly param-ctx)))
                               ; find-element anchors need more items in their ctx
                               (->> (partition 3 colspec)
                                    (mapv first) (set)      ; distinct find elements
                                    (mapcat (fn [fe-name]
                                              (let [entity (get relation fe-name)
                                                    param-ctx (entity-param-ctx entity param-ctx)
                                                    fe-anchors (->> (get find-element-anchors-lookup fe-name)
                                                                    (filter #(nil? (:anchor/attribute %))))]
                                                (mapv vector fe-anchors (repeat param-ctx))))))))]]))


(defn body [relations colspec repeating-anchors sort-col param-ctx]
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
                   [table-row-form relation colspec repeating-anchors param-ctx])))))])

(defn table [relations colspec anchors param-ctx]
  (let [sort-col (r/atom nil)]
    (fn [relations colspec anchors param-ctx]
      (let [non-repeating-top-anchors (->> anchors
                                           (remove :anchor/repeating?)
                                           (filter #(nil? (:anchor/find-element %)))
                                           (filter #(nil? (:anchor/attribute %))))]
        [:div.ui-table-with-links
         [:table.ui-table
          [:thead
           [:tr
            (build-col-heads colspec sort-col)
            [:td.link-cell {:key :link-cell}
             (widget/render-anchors (remove :anchor/render-inline? non-repeating-top-anchors) param-ctx)]]]
          [body relations colspec (filter :anchor/repeating? anchors) sort-col param-ctx]]
         (let [anchors (filter :anchor/render-inline? non-repeating-top-anchors)
               param-ctx (dissoc param-ctx :isComponent)]
           (widget/render-inline-links anchors param-ctx))]))))
