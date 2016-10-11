(ns hypercrud.ui.table
  (:require [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx]
            [hypercrud.form.option :as option]
            [hypercrud.form.q-util :as q-util]
            [hypercrud.ui.auto-control :refer [auto-table-cell]]
            [reagent.core :as r]))


(defn build-col-heads [form]
  (map (fn [{:keys [:ident :prompt]}]
         [:td {:key ident} prompt])
       (:form/field form)))


(defn build-row-cells [form-id entity {:keys [forms] :as fieldless-widget-args}]
  (->> (get forms form-id)
       :form/field
       (map (fn [{:keys [ident] :as field}]
              [:td.truncate {:key ident}
               [auto-table-cell entity form-id (-> fieldless-widget-args
                                                   (update :expanded-cur #(% [ident]))
                                                   (assoc :field field))]]))))


(defn table-row [{:keys [:db/id] :as entity} form-id retract-entity! show-links? {:keys [navigate-cmp] :as widget-args}]
  [:tr
   (if retract-entity!
     [:td.remove-row {:key "remove"}
      (if show-links?
        [:button {:on-click #(retract-entity! id)} "⌦"])])
   [:td.id {:key "edit-col"}
    (if show-links?
      [navigate-cmp {:href (str form-id "/entity/" id)} (if (neg? id) id (mod id 100))])]
   (build-row-cells form-id entity widget-args)
   [:td {:key "link-col"}
    (if show-links?
      "todo")]])


(defn body [graph eids forms queries form-id expanded-cur stage-tx! navigate-cmp retract-entity! add-entity!]
  [:tbody
   (->> eids
        (map #(hc/entity graph %))
        (map (fn [entity]
               ^{:key (:db/id entity)}
               [table-row entity form-id retract-entity! true {:expanded-cur (expanded-cur [(:db/id entity)])
                                                               :forms forms
                                                               :graph graph
                                                               :navigate-cmp navigate-cmp
                                                               :queries queries
                                                               :stage-tx! stage-tx!}])))
   (let [id (hc/*temp-id!*)]
     ^{:key (hash eids)}
     [table-row {:db/id id} form-id retract-entity! false {:expanded-cur (expanded-cur [id])
                                                           :forms forms
                                                           :graph graph
                                                           :navigate-cmp navigate-cmp
                                                           :queries queries
                                                           :stage-tx! (fn [tx]
                                                                        (add-entity! id)
                                                                        (stage-tx! tx))}])])


(defn table-managed [graph eids forms queries form-id expanded-cur stage-tx! navigate-cmp retract-entity! add-entity!]
  (let [form (get forms form-id)]
    [:table.ui-table
     [:colgroup [:col {:span "1" :style {:width "20px"}}]]
     [:thead
      [:tr
       (if retract-entity! [:td.remove-row {:key "remove-col"}])
       [:td {:key "edit-col"}]
       (build-col-heads form)
       [:td {:key "link-col"} "Links"]]]
     [body graph eids forms queries form-id expanded-cur stage-tx! navigate-cmp retract-entity! add-entity!]]))


(defn- table* [graph eids forms queries form-id expanded-cur stage-tx! navigate-cmp retract-entity!]
  ;; resultset tables don't appear managed from the call site, but we need to provide a fake add-entity!
  ;; so we can create-new inline like a spreadsheet, which means we internally use the managed table
  (let [new-entities (r/atom [])
        add-entity! #(swap! new-entities conj %)]
    (fn [graph eids forms queries form-id expanded-cur stage-tx! navigate-cmp retract-entity!]
      (let [retract-entity! (fn [id]
                              (if (tx/tempid? id)
                                (swap! new-entities remove id)
                                (retract-entity! id)))]
        [table-managed graph (concat eids @new-entities) forms queries form-id expanded-cur stage-tx! navigate-cmp retract-entity! add-entity!]))))


(defn table [graph eids forms queries form-id expanded-cur stage-tx! navigate-cmp retract-entity]
  ^{:key (hc/t graph)}
  [table* graph eids forms queries form-id expanded-cur stage-tx! navigate-cmp retract-entity])


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


(defn query [query p-filler param-ctx forms queries form-id]
  (let [form (get forms form-id)]
    (merge
      (option-queries queries form p-filler param-ctx)
      {::query [(:query/value query) (p-filler query param-ctx) (table-pull-exp form)]})))