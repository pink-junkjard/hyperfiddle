(ns hypercrud.ui.table
  (:require [hypercrud.browser.links :as links]
            [hypercrud.compile.eval :refer [eval]]
            [hypercrud.platform.native-event-listener :refer [native-listener]] ;provided dependency
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
                  [:td {:key (:db/id field) :on-click on-click}
                   prompt
                   (let [docstring (-> field :field/attribute :attribute/doc)]
                     (if-not (empty? docstring)
                       [native-listener {:on-click (fn [e]
                                                     (js/alert docstring)
                                                     (.stopPropagation e))}
                        [:span.help "ⓘ"]]))
                   [:span.sort-arrow arrow]]))))))


(defn build-row-cells [form entity {:keys [graph] :as fieldless-widget-args}]
  (->> (:form/field form)
       (sort-by :field/order)
       (map (fn [{:keys [:field/renderer] :as field}]
              [:td.truncate {:key (:db/id field)}
               (if (empty? renderer)
                 [auto-table-cell entity (-> fieldless-widget-args
                                             (assoc :field field))]
                 (let [{renderer :value error :error} (eval renderer)]
                   [:div.value
                    (if error
                      (pr-str error)
                      (try
                        (renderer graph entity)
                        (catch :default e (pr-str e))))]))]))))


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

(def unicode-nbsp "\u00a0")

(defn table-row [result forms {:keys [links navigate-cmp stage-tx!] :as fieldless-widget-args}]
  (let [{:keys [param-ctx] :as fieldless-widget-args} (assoc-in fieldless-widget-args [:param-ctx :result] result)]
    [:tr
     (mapcat (fn [form entity]
               (build-row-cells form entity fieldless-widget-args))
             forms result)
     [:td.link-cell {:key :link-cell}
      unicode-nbsp
      (->> links
           (filter #(nil? (:link/field %)))
           (map (fn [{:keys [:db/id :link/prompt] :as link}]
                  (let [props (assoc (links/query-link stage-tx! link param-ctx) :key id)]
                    (navigate-cmp props prompt))))
           (interpose " · "))]]))


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


(defn table [graph resultset forms links stage-tx! navigate-cmp param-ctx]
  (let [sort-col (r/atom nil)]
    (fn [graph resultset forms links stage-tx! navigate-cmp param-ctx]
      (let [repeating-links (filter :link/repeating? links)]
        [:table.ui-table
         [:thead
          [:tr
           (build-col-heads forms sort-col)
           [:td.link-cell {:key :link-cell}
            (->> (remove :link/repeating? links)
                 (filter #(nil? (:link/field %)))
                 (map (fn [link]
                        (let [props (links/query-link stage-tx! link param-ctx)]
                          ^{:key (:db/id link)}
                          [navigate-cmp props (:link/prompt link)])))
                 (interpose " · "))]]]
         [body graph resultset forms repeating-links stage-tx! navigate-cmp sort-col param-ctx]]))))
