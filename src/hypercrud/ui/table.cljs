(ns hypercrud.ui.table
  (:require [hypercrud.browser.links :as links]
            [hypercrud.compile.eval :refer [eval]]
            [hypercrud.platform.native-event-listener :refer [native-listener]] ;provided dependency
            [hypercrud.types :refer [->DbId ->DbVal]]
            [hypercrud.ui.auto-control :refer [auto-table-cell connection-color]]
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


(defn build-col-heads [ordered-find-elements col-sort]
  (let [[form-dbid' ident' direction] @col-sort]
    (->> ordered-find-elements
         (mapv :find-element/form)
         (mapcat (fn [{form-dbid :db/id fields :form/field :as form}]
                   (->> fields
                        (sort-by :field/order)
                        (mapv (juxt (constantly form-dbid) identity)))))
         (map (fn [[form-dbid {:keys [:field/prompt] :as field}]]
                (let [ident (-> field :field/attribute :attribute/ident)
                      with-sort-direction (fn [asc desc no-sort not-sortable]
                                            (if (sortable? field)
                                              (if (and (= form-dbid' form-dbid) (= ident' ident))
                                                (condp = direction
                                                  :asc asc
                                                  :desc desc)
                                                no-sort)
                                              not-sortable))
                      on-click (with-sort-direction #(reset! col-sort [form-dbid ident :desc])
                                                    #(reset! col-sort nil)
                                                    #(reset! col-sort [form-dbid ident :asc])
                                                    (constantly nil))
                      arrow (with-sort-direction " ↓" " ↑" " ↕" nil)]
                  [:td {:class (str "field-" (get-in field [:db/id :id])) :key (:db/id field) :on-click on-click}
                   prompt
                   (let [docstring (-> field :field/attribute :attribute/doc)]
                     (if-not (empty? docstring)
                       [native-listener {:on-click (fn [e]
                                                     (js/alert docstring)
                                                     (.stopPropagation e))}
                        [:span.help "ⓘ"]]))
                   [:span.sort-arrow arrow]]))))))


(defn build-row-cells [form entity links {:keys [super-graph navigate-cmp] :as param-ctx}]
  (let [repeating-links (->> links
                             (filter :link/repeating?)
                             (mapv (juxt :link/ident identity))
                             (into {}))
        link-fn (fn [ident label]
                  (let [link (get repeating-links ident)
                        props (links/query-link link param-ctx)]
                    [navigate-cmp props label]))
        param-ctx (assoc param-ctx :color ((:color-fn param-ctx) entity param-ctx)
                                   :owner ((:owner-fn param-ctx) entity param-ctx))
        style {:border-color (connection-color (:color param-ctx))}]
    (->> (:form/field form)
         (sort-by :field/order)
         (map (fn [{:keys [:field/renderer] :as field}]
                [:td.truncate {:key (:db/id field) :style style}
                 (if (empty? renderer)
                   [auto-table-cell entity field links param-ctx]
                   (let [{renderer :value error :error} (eval renderer)]
                     [:div.value
                      (if error
                        (pr-str error)
                        (try
                          (renderer super-graph link-fn entity)
                          (catch :default e (pr-str e))))]))])))))


;(defn links-cell [entity form repeating-links retract-entity! show-links? navigate-cmp]
;  (let [open? (r/atom false)]
;    (fn [entity form retract-entity! show-links? navigate-cmp]
;      (if @open?
;        [:div.link-menu
;         (if show-links?
;           (conj
;             (->> repeating-links
;                  (map (fn [{:keys [:link/ident :link/prompt] :as link}]
;                         (let [param-ctx {:user-profile hc/*user-profile*
;                                          :result ???}]
;                           (links/query-link link param-ctx #(navigate-cmp {:key ident :href %} prompt))))))
;             (if retract-entity!
;               [:span {:key "hypercrud-delete-row" :on-click #(retract-entity! (:db/id entity))} "Delete Row"])))
;         [:span {:key "close" :on-click #(reset! open? false)} "Close"]]
;        [:div {:on-click #(reset! open? true)} "⚙"]))))


(defn table-row [result ordered-find-elements links {:keys [navigate-cmp] :as param-ctx}]
  (let [param-ctx (assoc param-ctx :result result)]
    [:tr
     (mapcat (fn [find-element]
               (let [form (:find-element/form find-element)
                     entity (get result (:find-element/name find-element))]
                 (build-row-cells form entity links param-ctx)))
             ordered-find-elements)
     [:td.link-cell {:key :link-cell}
      (->> links
           (filter #(nil? (:link/field %)))
           (map (fn [{:keys [:db/id :link/prompt] :as link}]
                  (let [props (assoc (links/query-link link param-ctx) :key id)]
                    (navigate-cmp props prompt))))
           (interpose " · "))]]))


(defn body [resultset ordered-find-elements repeating-links sort-col param-ctx]
  [:tbody
   (let [[form-dbid sort-key direction] @sort-col
         sort-eids (fn [resultset]
                     (let [{form :find-element/form find-element-name :find-element/name}
                           (->> ordered-find-elements
                                (filter #(= form-dbid (-> % :find-element/form :db/id)))
                                first)
                           field (->> (:form/field form)
                                      (filter #(= sort-key (-> % :field/attribute :attribute/ident)))
                                      first)]
                       (if (and (not= nil field) (sortable? field))
                         (sort-by #(get-in % [find-element-name sort-key])
                                  (condp = direction
                                    :asc #(compare %1 %2)
                                    :desc #(compare %2 %1))
                                  resultset)
                         resultset)))]
     (->> resultset
          sort-eids
          (map (fn [result]
                 ^{:key (hash (util/map-values :db/id result))}
                 [table-row result ordered-find-elements repeating-links param-ctx]))))])


(defn table [resultset ordered-find-elements links param-ctx]
  (let [sort-col (r/atom nil)]
    (fn [resultset ordered-find-elements links {:keys [navigate-cmp] :as param-ctx}]
      (let [repeating-links (filter :link/repeating? links)]
        [:table.ui-table
         [:thead
          [:tr
           (build-col-heads ordered-find-elements sort-col)
           [:td.link-cell {:key :link-cell}
            (->> (remove :link/repeating? links)
                 (filter #(nil? (:link/field %)))
                 (map (fn [link]
                        (let [props (links/query-link link param-ctx)]
                          ^{:key (:db/id link)}
                          [navigate-cmp props (:link/prompt link)])))
                 (interpose " · "))]]]
         [body resultset ordered-find-elements repeating-links sort-col param-ctx]]))))
