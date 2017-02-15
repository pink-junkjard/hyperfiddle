(ns hypercrud.ui.table
  (:require [clojure.string :as string]
            [hypercrud.browser.links :as links]
            [hypercrud.compile.eval :refer [eval]]
            [hypercrud.platform.native-event-listener :refer [native-listener]] ;provided dependency
            [hypercrud.types :refer [->DbId ->DbVal]]
            [hypercrud.ui.auto-control :refer [auto-table-cell raw-table-cell connection-color]]
            [hypercrud.ui.renderer :as renderer]
            [hypercrud.ui.widget :as widget]
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
      (string/replace " " "-")
      (subs 1)))


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

                 [:td {:class (interpose " " css-classes) :key (str ident) :on-click on-click}
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

(defn table-row-form [result colspec repeating-anchors param-ctx]
  (let [find-element-anchors-lookup (->> repeating-anchors
                                         ; entity links can have fields but not find-elements specified
                                         (filter #(or (:anchor/find-element %) (:anchor/field %)))
                                         (group-by (fn [anchor]
                                                     (if-let [find-element (:anchor/find-element anchor)]
                                                       (:find-element/name find-element)
                                                       :entity))))]
    [:tr
     (->> (partition 3 colspec)
          (mapv (fn [[fe-name ident maybe-field]]
                  (let [entity (get result fe-name)
                        param-ctx (entity-param-ctx entity param-ctx)
                        param-ctx (assoc param-ctx :attribute (get (:schema param-ctx) ident))

                        ; rebuilt too much due to joining fe-name X ident
                        field-anchors (->> (get find-element-anchors-lookup fe-name)
                                           (remove #(nil? (:anchor/field %))))


                        style {:border-color (connection-color (:color param-ctx))}
                        value (get entity ident)]
                    [:td.truncate {:key (or (:db/id maybe-field) ident) :style style}
                     (if-let [renderer (renderer/renderer-for-attribute (:field/attribute maybe-field))]
                       (let [{renderer :value error :error} (eval renderer)]
                         [:div.value
                          (if error
                            (pr-str error)
                            (try
                              (let [link-fn (let [anchor-by-ident (->> field-anchors
                                                                       (mapv (juxt #(-> % :anchor/ident) identity))
                                                                       (into {}))]
                                              (fn [ident label param-ctx]
                                                (let [anchor (get anchor-by-ident ident)
                                                      props (links/build-link-props anchor param-ctx)]
                                                  [(:navigate-cmp param-ctx) props label param-ctx])))]
                                (renderer (:peer param-ctx) link-fn value))
                              (catch :default e (pr-str e))))])
                       (let [anchors (filter #(= (:db/id maybe-field) (some-> % :anchor/field :db/id)) field-anchors)]
                         (if (= (:display-mode param-ctx) :raw)
                           [raw-table-cell value anchors param-ctx]
                           [auto-table-cell value maybe-field anchors param-ctx])))])))
          (seq))

     [:td.link-cell {:key :link-cell}
      ; render all repeating links (regardless if inline) as anchors
      (widget/render-anchors (concat
                               (mapv vector
                                     (->> repeating-anchors
                                          (filter #(nil? (:anchor/find-element %)))
                                          (filter #(nil? (:anchor/field %))))
                                     (repeatedly (constantly param-ctx)))
                               ; find-element anchors need more items in their ctx
                               (->> (partition 3 colspec)
                                    (mapv first) (set)      ; distinct find elements
                                    (mapcat (fn [fe-name]
                                              (let [entity (get result fe-name)
                                                    param-ctx (entity-param-ctx entity param-ctx)
                                                    fe-anchors (->> (get find-element-anchors-lookup fe-name)
                                                                    (filter #(nil? (:anchor/field %))))]
                                                (mapv vector fe-anchors (repeat param-ctx))))))))]]))


(defn body [resultset colspec repeating-anchors sort-col param-ctx]
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
     (->> resultset
          #_sort-eids
          (map (fn [result]
                 (let [param-ctx (assoc param-ctx :result result)]
                   ^{:key (hash (util/map-values :db/id result))}
                   [table-row-form result colspec repeating-anchors param-ctx])))))])


; if raw mode, need to figure out the superset of attributes
; else, we have forms to tell us, maybe. It's allowed to be null.
; if we have a form, it is guaranteed consistent with the resultset pulled-tree shape.
; So can we ignore the form and always look at the resultset?
;; No because resultset includes db/id. If we have a form, use the form to drive the keyset.

; always have a FE :: :form/field, :field/order, :field/attribute, :attribute/ident

; If we don't have the forms e.g. raw mode
; How can we know which columns to render? How many columns are there?
; Need the superset of all entity keys per each find-element.
; Forms provide prompt, order and ident (for styling)
; Pass in a list of [ident prompt] already flattened and sorted?
(defn determine-colspec "Colspec is what you have when you flatten out the find elements,
but retaining and correlating all information, its like a join"
  [resultset ordered-find-elements]

  (mapcat (fn [resultset-for-fe fe]
            (let [indexed-fields (util/group-by-assume-unique (comp :attribute/ident :field/attribute) (-> fe :find-element/form :form/field))
                  find-element-name (ffirst resultset-for-fe)]
              (->> (map second resultset-for-fe)
                   (reduce (fn [acc v]
                             (into acc (keys v)))
                           #{})
                   ; sorting is per find-element, not overall
                   (sort-by (fn [k]
                              (if-let [field (get indexed-fields k)]
                                (:field/order field)
                                k #_ "raw mode sort is by namespaced attribute, per find-element")))
                   (mapcat (fn [k]
                             [find-element-name k (get indexed-fields k)])))))
          (util/transpose resultset)
          ordered-find-elements))

(defn table [resultset ordered-find-elements anchors param-ctx]
  (let [sort-col (r/atom nil)]
    (fn [resultset ordered-find-elements anchors param-ctx]
      (let [non-repeating-top-anchors (->> anchors
                                           (remove :anchor/repeating?)
                                           (filter #(nil? (:anchor/find-element %)))
                                           (filter #(nil? (:anchor/field %))))
            raw-mode? (= (:display-mode param-ctx) :raw)
            f (if raw-mode? #(dissoc % :find-element/form) identity) ; strip forms for raw mode even if we have them
            ordered-find-elements (map f ordered-find-elements)

            ; Not all entities are homogenous, especially consider the '* case,
            ; so we need a uniform column set driving the body rows in sync with the headers

            ; but the resultset needs to match this column-fields structure now too; since the find-element level
            ; has been flattened out of the columns.

            ; colspec is flattened triples- [fe-name, ident, field] ; field may be null
            ; ["?blog" :post/title field1
            ;  "?blog" :post/content field2
            ;  "?user" :user/name field3
            ;  "?user" :user/email field4]
            colspec (determine-colspec resultset ordered-find-elements)]
        [:div.ui-table-with-links
         [:table.ui-table
          [:thead
           [:tr
            (build-col-heads colspec sort-col)
            [:td.link-cell {:key :link-cell}
             (widget/render-anchors (remove :anchor/render-inline? non-repeating-top-anchors) param-ctx)]]]
          [body resultset colspec (filter :anchor/repeating? anchors) sort-col param-ctx]]
         (let [anchors (filter :anchor/render-inline? non-repeating-top-anchors)
               param-ctx (dissoc param-ctx :isComponent)]
           (widget/render-inline-links anchors param-ctx))]))))
