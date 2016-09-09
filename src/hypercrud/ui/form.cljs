(ns hypercrud.ui.form
  (:require [hypercrud.ui.auto-control :refer [auto-control]]
            [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx-util]
            [hypercrud.ui.form-util :as form-util]))


(defn field [{:keys [:attribute/ident :field/prompt] :as fieldinfo} graph entity forms expanded-cur local-transact! tempid!]
  (let [value (get entity ident)
        change! (fn [retracts adds]
                  (local-transact!
                    (vec (concat (map (fn [val] [:db/retract (:db/id entity) ident val]) retracts)
                                 (map (fn [val] [:db/add (:db/id entity) ident val]) adds)))))]
    [:div.field
     (if prompt [:label prompt])
     [auto-control fieldinfo graph forms value expanded-cur change! local-transact! tempid!]]))


(defn form [graph eid forms form-id expanded-cur local-transact! tempid!]
  (let [entity (hc/entity graph eid)]
    [:div.form
     (map (fn [{:keys [:attribute/ident] :as fieldinfo}]
            ^{:key ident}
            [field fieldinfo graph entity forms expanded-cur local-transact! tempid!])
          (get forms form-id))]))


(defn query [eid forms form-id expanded-forms]              ;bad abstraction/not an abstraction
  (let [q-and-params (if (not (tx-util/tempid? eid))
                       {:query '[:find [?eid ...] :in $ ?eid :where [?eid]]
                        :params [eid]})]
    (form-util/query forms form-id expanded-forms q-and-params)))
