(ns hypercrud.ui.table
  (:require [cljs.reader :as reader]
            [hypercrud.browser.links :as links]
            [hypercrud.client.core :as hc]
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
                         (let [param-ctx {:user-profile hc/*user-profile*
                                          :entity entity}]
                           (links/query-link link param-ctx #(navigate-cmp {:key ident :href %} prompt))))))
             (if retract-entity!
               [:span {:key "hypercrud-delete-row" :on-click #(retract-entity! (:db/id entity))} "Delete Row"])))
         [:span {:key "close" :on-click #(reset! open? false)} "Close"]]
        [:div {:on-click #(reset! open? true)} "⚙"]))))


(defn table-row [result forms retract-result! {:keys [navigate-cmp] :as fieldless-widget-args}]
  [:tr
   (conj
     (->> (mapcat :form/link forms)
          (map (fn [{:keys [:db/id :link/ident :link/prompt] :as link}]
                 (let [param-ctx {:dbval (-> (first result) .-dbgraph .-dbval) ;todo remove this hack
                                  :user-profile hc/*user-profile*
                                  :result result}]
                   [:td.link-cell {:key id}
                    (links/query-link link param-ctx #(navigate-cmp {:key ident :href %} prompt))]))))
     [:td.link-cell {:key "hypercrud-delete-row"}
      (if retract-result! [:button {:on-click #(retract-result! (mapv :db/id result))} "X"])])
   (mapcat (fn [form entity]
             (let [fieldless-widget-args (update fieldless-widget-args :expanded-cur (fn [cur] (cur [(.-dbid entity)])))]
               (build-row-cells form entity fieldless-widget-args)))
           forms result)])


(defn body [graph resultset new-entity-dbvals forms expanded-cur stage-tx! navigate-cmp retract-result! add-result sort-col]
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
          (map (fn [result]
                 ^{:key (hash (mapv :db/id result))}
                 [table-row result forms retract-result! {:expanded-cur expanded-cur
                                                          :graph graph
                                                          :navigate-cmp navigate-cmp
                                                          :stage-tx! stage-tx!}]))))
   (if (not= nil new-entity-dbvals)
     (let [new-result (mapv (fn [dbval]
                              (hc/entity (hc/get-dbgraph graph dbval) (hc/*temp-id!* (.-conn-id dbval))))
                            new-entity-dbvals)]
       ^{:key (hash resultset)}
       [table-row new-result forms nil {:expanded-cur expanded-cur
                                        :graph graph
                                        :navigate-cmp navigate-cmp
                                        :stage-tx! (fn [tx]
                                                     (let [tx (concat tx (add-result (mapv :db/id new-result)))]
                                                       (stage-tx! tx)))}]))])


(defn table-managed "a managed table can add/remove rows, a regular table can't. We thought
the managed table was more general, but now dustin thinks maybe they are two different ideas and cant be
combined. you can see the tension in how we have to adapt between them -- query results are resutls, managed
entity fields are just entities"
  [graph resultset new-entity-dbval forms expanded-cur stage-tx! navigate-cmp retract-result! add-result]
  (let [sort-col (r/atom nil)]
    (fn [graph resultset new-entity-dbval forms expanded-cur stage-tx! navigate-cmp retract-result! add-result]
      [:table.ui-table
       [:colgroup [:col {:span "1" :style {:width "20px"}}]]
       [:thead
        [:tr
         (->> (mapcat :form/link forms)
              (map (fn [{:keys [:db/id] :as link}]
                     [:td.link-cell {:key id}])))
         [:td.link-cell {:key "hypercrud-delete-row-link"}]
         (build-col-heads forms sort-col)]]
       [body graph resultset new-entity-dbval forms expanded-cur stage-tx! navigate-cmp retract-result! add-result sort-col]])))


(defn table [graph resultset forms expanded-cur stage-tx! navigate-cmp]
  ^{:key (hc/t graph)}
  [table-managed graph resultset nil forms expanded-cur stage-tx! navigate-cmp nil nil])


(defn table-pull-exp [form]
  (concat
    [:db/id]
    (map (fn [{{:keys [:attribute/ident :attribute/valueType :attribute/isComponent]} :field/attribute
               {find-elements :link/find-element} :field/options-link
               :as field}]
           (if (or isComponent (= (:db/ident valueType) :db.type/ref))
             ; components and refs render nested entities, so pull another level deeper
             {ident (conj (->> find-elements
                               (mapcat #(-> % :find-element/form :form/field))
                               (mapv #(-> % :field/attribute :attribute/ident)))
                          :db/id)}

             ; otherwise just add the attribute to the list
             ident))
         (:form/field form))))


(defn field-queries [p-filler param-ctx field]
  (let [{:keys [:attribute/cardinality :attribute/valueType :attribute/isComponent]} (:field/attribute field)]
    (if (and (= (:db/ident valueType) :db.type/ref)
             (= (:db/ident cardinality) :db.cardinality/one)
             (not isComponent))
      (option/get-query field p-filler param-ctx))))


(defn option-queries [form p-filler param-ctx]
  (apply merge
         (map #(field-queries p-filler param-ctx %)
              (:form/field form))))

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
