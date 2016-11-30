(ns hypercrud.ui.table
  (:require [cljs.reader :as reader]
            [hypercrud.browser.links :as links]
            [hypercrud.client.core :as hc]
            [hypercrud.compile.eval :refer [eval]]
            [hypercrud.form.option :as option]
            [hypercrud.types :refer [->DbId ->DbVal]]
            [hypercrud.ui.auto-control :refer [auto-table-cell]]
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


(defn build-col-heads [forms col-sort]
  (let [[form-dbid' ident' direction] @col-sort]
    (->> (mapcat (fn [{form-dbid :db/id fields :form/field :as form}]
                   (->> fields
                        (sort-by :field/order)
                        (mapv (juxt (constantly form-dbid) identity))))
                 forms)
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
                  [:td {:key ident :on-click on-click} prompt arrow]))))))


(defn build-row-cells [form entity {:keys [graph] :as fieldless-widget-args}]
  (->> (:form/field form)
       (sort-by :field/order)
       (map (fn [{:keys [:field/renderer] :as field}]
              (let [ident (-> field :field/attribute :attribute/ident)]
                [:td.truncate {:key ident}
                 (if (empty? renderer)
                   [auto-table-cell entity (-> fieldless-widget-args
                                               (assoc :field field))]
                   (let [{renderer :value error :error} (eval renderer)]
                     [:div.value
                      (if error
                        (pr-str error)
                        (try
                          (renderer graph entity)
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
;                                          :entity entity}]
;                           (links/query-link link param-ctx #(navigate-cmp {:key ident :href %} prompt))))))
;             (if retract-entity!
;               [:span {:key "hypercrud-delete-row" :on-click #(retract-entity! (:db/id entity))} "Delete Row"])))
;         [:span {:key "close" :on-click #(reset! open? false)} "Close"]]
;        [:div {:on-click #(reset! open? true)} "⚙"]))))


(defn table-row [result forms {:keys [links navigate-cmp stage-tx!] :as fieldless-widget-args}]
  (let [{:keys [param-ctx] :as fieldless-widget-args} (assoc-in fieldless-widget-args [:param-ctx :result] result)]
    [:tr
     [:td.link-cell {:key :link-cell}
      (->> links
           (filter #(nil? (:link/field %)))
           (map (fn [{:keys [:db/id :link/prompt] :as link}]
                  (let [props (assoc (links/query-link stage-tx! link param-ctx) :key id)]
                    (navigate-cmp props prompt))))
           (interpose " · "))]
     (mapcat (fn [form entity]
               (build-row-cells form entity fieldless-widget-args))
             forms result)]))


(defn body [graph resultset forms repeating-links stage-tx! navigate-cmp sort-col param-ctx]
  [:tbody
   (let [[form-dbid sort-key direction] @sort-col
         sort-eids (fn [resultset]
                     (let [[index form] (->> forms
                                             (map-indexed vector)
                                             (filter #(= form-dbid (-> % second :db/id)))
                                             first)
                           field (->> (:form/field form)
                                      (filter #(= sort-key (-> % :field/attribute :attribute/ident)))
                                      first)]
                       (if (and (not= nil field) (sortable? field))
                         (sort-by #(get-in % [index sort-key])
                                  (condp = direction
                                    :asc #(compare %1 %2)
                                    :desc #(compare %2 %1))
                                  resultset)
                         resultset)))]
     (->> resultset
          sort-eids
          (map (fn [result]
                 ^{:key (hash (mapv :db/id result))}
                 [table-row result forms {:graph graph
                                          :links repeating-links
                                          :navigate-cmp navigate-cmp
                                          :param-ctx param-ctx
                                          :stage-tx! stage-tx!}]))))])


(defn table [graph resultset forms repeating-links stage-tx! navigate-cmp param-ctx]
  (let [sort-col (r/atom nil)]
    (fn [graph resultset forms repeating-links stage-tx! navigate-cmp param-ctx]
      [:table.ui-table
       [:colgroup [:col {:span "1" :style {:width "20px"}}]]
       [:thead
        [:tr
         [:td.link-cell {:key :link-cell}]
         (build-col-heads forms sort-col)]]
       [body graph resultset forms repeating-links stage-tx! navigate-cmp sort-col param-ctx]])))


(defn table-pull-exp [form]
  (concat
    [:db/id]
    (mapv #(-> % :field/attribute :attribute/ident) (:form/field form))))


(defn field-queries [p-filler param-ctx field]
  (let [{:keys [:attribute/cardinality :attribute/valueType :attribute/isComponent]} (:field/attribute field)]
    (if (and (= (:db/ident valueType) :db.type/ref)
             (= (:db/ident cardinality) :db.cardinality/one)
             (not isComponent))
      (option/get-query field p-filler param-ctx))))


(defn option-queries [form p-filler param-ctx]
  (apply merge
         (mapv #(field-queries p-filler param-ctx %) (:form/field form))))

(defn query
  ([p-filler param-ctx link]
   (query p-filler param-ctx (:link/query link) (:link/find-element link) (:link/formula link)))
  ([p-filler param-ctx query find-elements formula]
   (let [app-dbval (get param-ctx :dbval)]
     (merge
       {(.-dbid query) [(reader/read-string (:query/value query))
                        (p-filler query formula param-ctx)
                        (->> find-elements
                             (mapv (juxt :find-element/name (fn [find-element]
                                                              [app-dbval (table-pull-exp (:find-element/form find-element))])))
                             (into {}))]}
       (->> find-elements
            (mapv #(option-queries (:find-element/form %) p-filler param-ctx))
            (apply merge))))))
