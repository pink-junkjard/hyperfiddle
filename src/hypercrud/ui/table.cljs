(ns hypercrud.ui.table
  (:require [cljs.reader :as reader]
            [hypercrud.browser.links :as links]
            [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx]
            [hypercrud.compile.eval :as eval]
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
  (let [[ident' direction] @col-sort]
    (->> (mapcat :form/field forms)
         (map (fn [{:keys [:field/prompt] :as field}]
                (let [ident (-> field :field/attribute :attribute/ident)
                      with-sort-direction (fn [asc desc no-sort not-sortable]
                                            (if (sortable? field)
                                              (if (= ident' ident)
                                                (condp = direction
                                                  :asc asc
                                                  :desc desc)
                                                no-sort)
                                              not-sortable))
                      on-click (with-sort-direction #(reset! col-sort [ident :desc])
                                                    #(reset! col-sort nil)
                                                    #(reset! col-sort [ident :asc])
                                                    (constantly nil))
                      arrow (with-sort-direction " ↓" " ↑" " ↕" nil)]
                  [:td {:key ident :on-click on-click} prompt arrow]))))))


(defn build-row-cells [form entity {:keys [graph] :as fieldless-widget-args}]
  (->> (:form/field form)
       (map (fn [{:keys [:field/renderer] :as field}]
              (let [ident (-> field :field/attribute :attribute/ident)]
                [:td.truncate {:key ident}
                 (if (empty? renderer)
                   [auto-table-cell entity (-> fieldless-widget-args
                                               (update :expanded-cur #(% [ident]))
                                               (assoc :field field))]
                   (let [{renderer :value error :error} (eval/uate (str "(identity " renderer ")"))]
                     [:div.value
                      (if error
                        (pr-str error)
                        (try
                          (renderer graph entity)
                          (catch :default e (pr-str e))))]))])))))


(defn links-cell [entity form retract-entity! show-links? navigate-cmp]
  (let [open? (r/atom false)]
    (fn [entity form retract-entity! show-links? navigate-cmp]
      (if @open?
        [:div.link-menu
         (if show-links?
           (conj
             (->> (:form/link form)
                  (map (fn [{:keys [:link/ident :link/prompt] :as link}]
                         (let [param-ctx (merge {:user-profile hc/*user-profile*} {:entity entity})]
                           (links/query-link link param-ctx #(navigate-cmp {:key ident :href %} prompt))))))
             #_(links/entity-link (:db/id form) (:db/id entity) #(navigate-cmp {:key "hypercrud-entity-view" :href %} "Entity View"))
             (if retract-entity!
               [:span {:key "hypercrud-delete-row" :on-click #(retract-entity! (:db/id entity))} "Delete Row"])))
         [:span {:key "close" :on-click #(reset! open? false)} "Close"]]
        [:div {:on-click #(reset! open? true)} "⚙"]))))


(defn table-row [result forms retract-entity! show-links? {:keys [navigate-cmp] :as fieldless-widget-args}]
  [:tr
   (conj
     (->> (mapcat :form/link forms)
          (map (fn [{:keys [:db/id :link/ident :link/prompt] :as link}]
                 (let [param-ctx {:user-profile hc/*user-profile*
                                  :result result}]
                   [:td.link-cell {:key id}
                    (links/query-link link param-ctx #(navigate-cmp {:key ident :href %} prompt))]))))
     [:td.link-cell {:key "hypercrud-entity-view"}
        #_(links/entity-link (:db/id form) (:db/id entity) #(navigate-cmp {:href %} "row"))]
     [:td.link-cell {:key "hypercrud-delete-row"}
        #_(if retract-entity! [:button {:on-click #(retract-entity! (:db/id entity))} "X"])])
   (mapcat #(build-row-cells %1 %2 fieldless-widget-args) forms result)])


(defn body [graph resultset new-entity-dbval forms expanded-cur stage-tx! navigate-cmp retract-entity! add-entity! sort-col]
  [:tbody
   (let [[sort-key direction] @sort-col
         ;sort-eids
         #_(fn [col]
             (let [field (->> (:form/field form)
                              (filter #(= sort-key (-> % :field/attribute :attribute/ident)))
                              first)]
               (if (and (not= nil field) (sortable? field))
                 (sort-by sort-key
                          (condp = direction
                            :asc #(compare %1 %2)
                            :desc #(compare %2 %1))
                          col)
                 col)))]
     (->> resultset
          ;sort-eids
          (map (fn [[entity :as result]]
                 ^{:key (hash (:db/id entity))}
                 [table-row result forms retract-entity! true {:expanded-cur (expanded-cur [(:db/id entity)])
                                                               :graph graph
                                                               :navigate-cmp navigate-cmp
                                                               :stage-tx! stage-tx!}]))))
   #_(if (not= nil new-entity-dbval)
       (let [entity (hc/entity (hc/get-dbgraph graph new-entity-dbval)
                               (hc/*temp-id!* (.-conn-id new-entity-dbval)))]
         ^{:key (hash entities)}
         [table-row entity form retract-entity! false {:expanded-cur (expanded-cur [(.-dbid entity)])
                                                       :graph graph
                                                       :navigate-cmp navigate-cmp
                                                       :stage-tx! (fn [tx]
                                                                    (add-entity! (.-dbid entity))
                                                                    (stage-tx! tx))}]))])


(defn table-managed [graph resultset new-entity-dbval forms expanded-cur stage-tx! navigate-cmp retract-entity! add-entity!]
  (let [sort-col (r/atom nil)]
    (fn [graph resultset new-entity-dbval forms expanded-cur stage-tx! navigate-cmp retract-entity! add-entity!]
      [:table.ui-table
       [:colgroup [:col {:span "1" :style {:width "20px"}}]]
       [:thead
        [:tr
         (->> (mapcat :form/link forms)
              (map (fn [{:keys [:db/id] :as link}]
                     [:td.link-cell {:key id}])))
         [:td.link-cell {:key "hypercrud-entity-view-link"}]
         [:td.link-cell {:key "hypercrud-delete-row-link"}]
         (build-col-heads forms sort-col)]]
       [body graph resultset new-entity-dbval forms expanded-cur stage-tx! navigate-cmp retract-entity! add-entity! sort-col]])))


(defn- table* [graph resultset new-entity-dbval forms expanded-cur stage-tx! navigate-cmp retract-entity!]
  ;; resultset tables don't appear managed from the call site, but we need to provide a fake add-entity!
  ;; so we can create-new inline like a spreadsheet, which means we internally use the managed table
  (let [new-entities (r/atom [])
        add-entity! #(swap! new-entities conj %)]
    (fn [graph resultset new-entity-dbval forms expanded-cur stage-tx! navigate-cmp retract-entity!]
      (let [retract-entity! (fn [dbid]
                              (if (tx/tempid? dbid)
                                (swap! new-entities (fn [old]
                                                      ; need to maintain same datastructure for conj
                                                      (vec (remove #(= % dbid) old))))
                                (if retract-entity!
                                  (retract-entity! dbid)
                                  (js/alert "todo"))))
            ;entities (concat entities (map #(hc/entity (hc/get-dbgraph graph new-entity-dbval) %) @new-entities))
            ]
        [table-managed graph resultset new-entity-dbval forms expanded-cur stage-tx! navigate-cmp retract-entity! add-entity!]))))


(defn table [graph resultset forms new-entity-dbval expanded-cur stage-tx! navigate-cmp retract-entity!]
  ^{:key (hc/t graph)}
  [table* graph resultset new-entity-dbval forms expanded-cur stage-tx! navigate-cmp retract-entity!])


(defn table-pull-exp [form]
  (concat
    [:db/id]
    (map (fn [field]
           (let [{:keys [:attribute/ident :attribute/valueType :attribute/isComponent]} (:field/attribute field)]
             (if (or isComponent (= (:db/ident valueType) :db.type/ref))
               ; components and refs render nested entities, so pull another level deeper
               {ident (if-let [label-prop (:field/label-prop field)]
                        [:db/id label-prop]
                        [:db/id])}

               ; otherwise just add the attribute to the list
               ident)))
         (:form/field form))))


(defn field-queries [p-filler param-ctx field]
  (let [{:keys [:attribute/cardinality :attribute/valueType :attribute/isComponent]} (:field/attribute field)
        options (option/gimme-useful-options field)]
    (if (and (= (:db/ident valueType) :db.type/ref)
             (= (:db/ident cardinality) :db.cardinality/one)
             (not isComponent)
             (not (option/has-holes? options)))
      (option/get-query options p-filler param-ctx))))


(defn option-queries [form p-filler param-ctx]
  (apply merge
         (map #(field-queries p-filler param-ctx %)
              (:form/field form))))

(defn query
  ([p-filler param-ctx link]
   (query p-filler param-ctx (:link/query link) (:link/find-element link) (:link/formula link)))
  ([p-filler param-ctx query find-elements formula]
   (let [app-dbval (get param-ctx :dbval)]
     (->> find-elements
          (map (fn [{find-name :find-element/name form :find-element/form :as find-element}]
                 (merge
                   (option-queries form p-filler param-ctx)
                   {::query [(reader/read-string (:query/value query))
                             (p-filler query formula param-ctx)
                             {find-name [app-dbval (table-pull-exp form)]}]})))
          (apply merge)))))
