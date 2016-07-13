(ns hypercrud.ui.form
  (:require [hypercrud.ui.auto-control :refer [auto-control]]
            [hypercrud.client.reagent :as hcr]))


(defn cj-form-field [{:keys [name prompt] :as fieldinfo} client forms value change! transact!]
  [:div.cj-field {:key (str name)}
   [:label prompt]
   [auto-control fieldinfo client forms value change! transact!]])


(defn cj-form [client eid forms local-transact!]
  [hcr/entity client eid
   (fn [entity]
     (assert (:meta/type entity) (str "Entity missing :meta/type " (pr-str entity)))
     [:div.cj-form
      (doall
        (map (fn [{:keys [name] :as fieldinfo}]
               (let [value (get entity name)
                     change! (fn [& op-vals] (local-transact! (mapv (fn [[op val]] [op eid name val]) op-vals)))]
                 ^{:key name}
                 [cj-form-field fieldinfo client forms value change! local-transact!]))
             ((:meta/type entity) forms)))])])
