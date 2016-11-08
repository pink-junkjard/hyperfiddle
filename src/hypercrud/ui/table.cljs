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


(defn build-col-heads [form col-sort]
  (let [[ident' direction] @col-sort]
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
             [:td {:key ident :on-click on-click} prompt arrow]))
         (:form/field form))))


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
                         (let [param-ctx (merge {:user-profile hc/*user-profile*} {:entity entity})
                               href (links/query-link link param-ctx)]
                           (navigate-cmp {:key ident :href href} prompt)))))
             (let [href (links/entity-link (:db/id form) (:db/id entity))]
               (navigate-cmp {:key "view" :href href} "Entity View"))
             (if retract-entity!
               [:span {:key "delete" :on-click #(retract-entity! (:db/id entity))} "Delete Row"])))
         [:span {:key "close" :on-click #(reset! open? false)} "Close"]]
        [:div {:on-click #(reset! open? true)} "⚙"]))))


(defn table-row [entity form retract-entity! show-links? {:keys [navigate-cmp] :as fieldless-widget-args}]
  [:tr
   [:td.link-cell {:key "links"}
    [links-cell entity form retract-entity! show-links? navigate-cmp]]
   (build-row-cells form entity fieldless-widget-args)])


(defn body [graph entities new-entity-dbval form expanded-cur stage-tx! navigate-cmp retract-entity! add-entity! sort-col]
  [:tbody
   (let [[sort-key direction] @sort-col
         sort-eids (fn [col]
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
     (->> entities
          sort-eids
          (map (fn [entity]
                 ^{:key (hash (:db/id entity))}
                 [table-row entity form retract-entity! true {:expanded-cur (expanded-cur [(:db/id entity)])
                                                              :graph graph
                                                              :navigate-cmp navigate-cmp
                                                              :stage-tx! stage-tx!}]))))
   (if (not= nil new-entity-dbval)
     (let [entity (hc/entity (hc/get-dbgraph graph new-entity-dbval)
                             (hc/*temp-id!* (.-conn-id new-entity-dbval)))]
       ^{:key (hash entities)}
       [table-row entity form retract-entity! false {:expanded-cur (expanded-cur [(.-dbid entity)])
                                                     :graph graph
                                                     :navigate-cmp navigate-cmp
                                                     :stage-tx! (fn [tx]
                                                                  (add-entity! (.-dbid entity))
                                                                  (stage-tx! tx))}]))])


(defn table-managed [graph entities new-entity-dbval form expanded-cur stage-tx! navigate-cmp retract-entity! add-entity!]
  (let [sort-col (r/atom nil)]
    (fn [graph entities new-entity-dbval form expanded-cur stage-tx! navigate-cmp retract-entity! add-entity!]
      [:table.ui-table
       [:colgroup [:col {:span "1" :style {:width "20px"}}]]
       [:thead
        [:tr
         [:td.link-cell {:key "links"}]
         (build-col-heads form sort-col)]]
       [body graph entities new-entity-dbval form expanded-cur stage-tx! navigate-cmp retract-entity! add-entity! sort-col]])))


(defn- table* [graph entities new-entity-dbval form expanded-cur stage-tx! navigate-cmp retract-entity!]
  ;; resultset tables don't appear managed from the call site, but we need to provide a fake add-entity!
  ;; so we can create-new inline like a spreadsheet, which means we internally use the managed table
  (let [new-entities (r/atom [])
        add-entity! #(swap! new-entities conj %)]
    (fn [graph entities new-entity-dbval form expanded-cur stage-tx! navigate-cmp retract-entity!]
      (let [retract-entity! (fn [dbid]
                              (if (tx/tempid? dbid)
                                (swap! new-entities (fn [old]
                                                      ; need to maintain same datastructure for conj
                                                      (vec (remove #(= % dbid) old))))
                                (if retract-entity!
                                  (retract-entity! dbid)
                                  (js/alert "todo"))))
            entities (concat entities (map #(hc/entity (hc/get-dbgraph graph new-entity-dbval) %) @new-entities))]
        [table-managed graph entities new-entity-dbval form expanded-cur stage-tx! navigate-cmp retract-entity! add-entity!]))))


(defn table [graph entities form new-entity-dbval expanded-cur stage-tx! navigate-cmp retract-entity!]
  ^{:key (hc/t graph)}
  [table* graph entities new-entity-dbval form expanded-cur stage-tx! navigate-cmp retract-entity!])


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
   (query p-filler param-ctx (:link/query link) (:link/form link) (:link/formula link)))
  ([p-filler param-ctx query form formula]
   (let [app-dbval (get param-ctx :dbval)]
     (merge
       (option-queries form p-filler param-ctx)
       {::query [(reader/read-string (:query/value query)) (p-filler query formula param-ctx) [app-dbval (table-pull-exp form)]]}))))
