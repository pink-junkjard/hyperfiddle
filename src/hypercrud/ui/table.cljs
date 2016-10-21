(ns hypercrud.ui.table
  (:require [hypercrud.browser.links :as links]
            [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx]
            [hypercrud.compile.eval :as eval]
            [hypercrud.form.option :as option]
            [hypercrud.ui.auto-control :refer [auto-table-cell]]
            [reagent.core :as r]))


(defn sortable? [{:keys [valueType cardinality] :as field}]
  (and
    (= cardinality :db.cardinality/one)
    ; ref requires more work (inspect label-prop)
    (contains? #{:keyword
                 :string
                 :boolean
                 :long
                 :bigint
                 :float
                 :double
                 :bigdec
                 :instant
                 :uuid
                 :uri
                 :bytes
                 :code}
               valueType)))


(defn build-col-heads [form col-sort]
  (let [[ident' direction] @col-sort]
    (map (fn [{:keys [ident prompt] :as field}]
           (let [with-sort-direction (fn [asc desc no-sort not-sortable]
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
       (map (fn [{:keys [ident renderer] :as field}]
              [:td.truncate {:key ident}
               (if (empty? renderer)
                 [auto-table-cell entity (:db/id form) (-> fieldless-widget-args
                                                           (update :expanded-cur #(% [ident]))
                                                           (assoc :field field))]
                 (let [{renderer :value error :error} (eval/uate (str "(identity " renderer ")"))]
                   [:div.value
                    (if error
                      (pr-str error)
                      (try
                        (renderer graph entity)
                        (catch :default e (pr-str e))))]))]))))


(defn links-cell [entity form retract-entity! show-links? queries navigate-cmp]
  (let [open? (r/atom false)]
    (fn [entity form retract-entity! show-links? queries navigate-cmp]
      (if @open?
        [:div.link-menu
         (if show-links?
           (conj
             (->> (:form/link form)
                  (map (fn [{:keys [:link/ident :link/prompt] :as link}]
                         (let [param-ctx (merge {:user-profile hc/*user-profile*} {:entity entity})
                               href (links/query-link link queries param-ctx)]
                           (navigate-cmp {:key ident :href href} prompt)))))
             (let [href (links/entity-link (:db/id form) (:db/id entity))]
               (navigate-cmp {:key "view" :href href} "Entity View"))
             (if retract-entity!
               [:span {:key "delete" :on-click #(retract-entity! (:db/id entity))} "Delete Row"])))
         [:span {:key "close" :on-click #(reset! open? false)} "Close"]]
        [:div {:on-click #(reset! open? true)} "⚙"]))))


(defn table-row [entity form retract-entity! show-links? {:keys [queries navigate-cmp] :as fieldless-widget-args}]
  [:tr
   [:td.link-cell {:key "links"}
    [links-cell entity form retract-entity! show-links? queries navigate-cmp]]
   (build-row-cells form entity fieldless-widget-args)])


(defn body [graph dbval eids forms queries form expanded-cur stage-tx! navigate-cmp retract-entity! add-entity! sort-col]
  [:tbody
   (let [[sort-key direction] @sort-col
         sort-eids (fn [col]
                     (let [field (->> (:form/field form)
                                      (filter #(= sort-key (:ident %)))
                                      first)]
                       (if (and (not= nil field) (sortable? field))
                         (sort-by sort-key
                                  (condp = direction
                                    :asc #(compare %1 %2)
                                    :desc #(compare %2 %1))
                                  col)
                         col)))]
     (->> eids
          (map #(hc/entity graph dbval %))
          sort-eids
          (map (fn [entity]
                 ^{:key (hash (:db/id entity))}
                 [table-row entity form retract-entity! true {:expanded-cur (expanded-cur [(:db/id entity)])
                                                              :forms forms
                                                              :graph graph
                                                              :navigate-cmp navigate-cmp
                                                              :queries queries
                                                              :stage-tx! stage-tx!}]))))
   (let [dbid (hc/*temp-id!*)
         entity (with-meta {:db/id dbid}
                           {:dbval dbval})]
     ^{:key (hash eids)}
     [table-row entity form retract-entity! false {:expanded-cur (expanded-cur [dbid])
                                                   :forms forms
                                                   :graph graph
                                                   :navigate-cmp navigate-cmp
                                                   :queries queries
                                                   :stage-tx! (fn [tx]
                                                                (add-entity! dbid)
                                                                (stage-tx! tx))}])])


(defn table-managed [graph dbval eids forms queries form-id expanded-cur stage-tx! navigate-cmp retract-entity! add-entity!]
  (let [sort-col (r/atom nil)]
    (fn [graph dbval eids forms queries form-id expanded-cur stage-tx! navigate-cmp retract-entity! add-entity!]
      (let [form (get forms form-id)]
        [:table.ui-table
         [:colgroup [:col {:span "1" :style {:width "20px"}}]]
         [:thead
          [:tr
           [:td.link-cell {:key "links"}]
           (build-col-heads form sort-col)]]
         [body graph dbval eids forms queries form expanded-cur stage-tx! navigate-cmp retract-entity! add-entity! sort-col]]))))


(defn- table* [graph dbval eids forms queries form-id expanded-cur stage-tx! navigate-cmp retract-entity!]
  ;; resultset tables don't appear managed from the call site, but we need to provide a fake add-entity!
  ;; so we can create-new inline like a spreadsheet, which means we internally use the managed table
  (let [new-entities (r/atom [])
        add-entity! #(swap! new-entities conj %)]
    (fn [graph dbval eids forms queries form-id expanded-cur stage-tx! navigate-cmp retract-entity!]
      (let [retract-entity! (fn [dbid]
                              (if (tx/tempid? dbid)
                                (swap! new-entities (fn [old]
                                                      ; need to maintain same datastructure for conj
                                                      (vec (remove #(= % dbid) old))))
                                (if retract-entity!
                                  (retract-entity! dbid)
                                  (js/alert "todo"))))]
        [table-managed graph dbval (concat eids @new-entities) forms queries form-id expanded-cur stage-tx! navigate-cmp retract-entity! add-entity!]))))


(defn table [graph dbval eids forms queries form-id expanded-cur stage-tx! navigate-cmp retract-entity!]
  ^{:key (:t dbval)}
  [table* graph dbval eids forms queries form-id expanded-cur stage-tx! navigate-cmp retract-entity!])


(defn table-pull-exp [form]
  (concat
    [:db/id]
    (map (fn [{:keys [ident valueType isComponent options] :as field}]
           (if (or isComponent (= valueType :ref))
             ; components and refs render nested entities, so pull another level deeper
             {ident [:db/id (option/label-prop options)]}

             ; otherwise just add the attribute to the list
             ident))
         (:form/field form))))


(defn field-queries [queries p-filler param-ctx
                     {:keys [cardinality valueType isComponent options] :as field}]
  (if (and (= valueType :ref)
           (= cardinality :db.cardinality/one)
           (not isComponent)
           (not (option/has-holes? options queries)))
    (option/get-query options queries p-filler (option/label-prop options) param-ctx)))


(defn option-queries [queries form p-filler param-ctx]
  (apply merge
         (map #(field-queries queries p-filler param-ctx %)
              (:form/field form))))

(defn query [forms queries p-filler param-ctx {:keys [:link/query :link/form :link/formula] :as link}]
  (let [form (get forms form)
        query (get queries query)
        dbval (get param-ctx :dbval)]
    (merge
      (option-queries queries form p-filler param-ctx)
      {::query [(:query/value query) (p-filler query formula param-ctx) [dbval (table-pull-exp form)]]})))
