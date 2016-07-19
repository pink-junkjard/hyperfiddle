(ns hypercrud.ui.form
  (:require [hypercrud.ui.auto-control :refer [auto-control]]
            [hypercrud.client.core :as hc]))


(defn cj-form-field [{:keys [name prompt] :as fieldinfo} graph metatype forms value change! transact! tempid!]
  [:div.cj-field
   [:label prompt]
   [auto-control fieldinfo graph metatype forms value change! transact! tempid!]])


(defn cj-form [graph eid metatype forms local-transact! tempid!]
  (let [entity (hc/entity graph eid)]
    [:div.cj-form
     (map (fn [{:keys [name] :as fieldinfo}]
            (let [value (get entity name)
                  change! (fn [& op-vals] (local-transact! (mapv (fn [[op val]] [op eid name val]) op-vals)))]
              ^{:key name}
              [cj-form-field fieldinfo graph metatype forms value change! local-transact! tempid!]))
          (metatype forms))]))


(defn form-dependencies [eid form]
  (->> form
       (filter (fn [{:keys [datatype]}] (= datatype :ref)))
       (map (fn [{{q :query} :options}]
              (let [query-name (hash q)]
                [query-name [q [] '[*]]])))
       (into {::query ['[:find [?eid ...] :in $ ?eid :where [?eid]]
                       [(js/parseInt eid 10)]
                       (concat [:db/id] (map :name form))]})))
