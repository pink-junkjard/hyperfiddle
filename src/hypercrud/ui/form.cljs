(ns hypercrud.ui.form
  (:require [hypercrud.ui.auto-control :refer [auto-control]]
            [hypercrud.client.core :as hc]))


(defn cj-form-field [{:keys [name prompt] :as fieldinfo} graph forms value change! transact!]
  [:div.cj-field
   [:label prompt]
   [auto-control fieldinfo graph forms value change! transact!]])


(defn cj-form [graph eid forms local-transact!]
  (let [entity (hc/entity graph eid)]
    (assert (:meta/type entity) (str "Entity missing :meta/type " (pr-str entity)))
    [:div.cj-form
     (doall
       (map (fn [{:keys [name] :as fieldinfo}]
              (let [value (get entity name)
                    change! (fn [& op-vals] (local-transact! (mapv (fn [[op val]] [op eid name val]) op-vals)))]
                ^{:key name}
                [cj-form-field fieldinfo graph forms value change! local-transact!]))
            ((:meta/type entity) forms)))]))
