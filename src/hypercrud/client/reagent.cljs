(ns hypercrud.client.reagent
  (:require [hypercrud.client.core :as hypercrud]
            [promesa.core :as p]
            [reagent.core :as reagent]))


(defmulti promised (fn [promise comp & [loading-comp]] :default))


(defmethod promised :default [promise comp & [loading-comp]]
  (let [cmp (reagent/current-component)]
    (fn [promise comp & [loading-comp]]
      (cond
        (p/resolved? promise) (comp (p/extract promise))
        (p/rejected? promise) [:pre (p/extract promise)]
        :pending? (do
                    ;; ignore val since hypercrud will rebuild the promise and it will be resolved next time
                    (p/then promise (fn [val] (reagent/force-update cmp)))
                    (if loading-comp (loading-comp) [:div "loading"]))))))


(defn entity [client eid comp & [loading-comp]]
  [promised (hypercrud/entity* client eid) comp loading-comp])


(defn query
  ([client q comp] (query client q [] comp))
  ([client q query-params comp & [loading-comp]]
   [promised (hypercrud/query* client q query-params) comp loading-comp]))


(defn enter [client comp & [loading-comp]]
  [promised (hypercrud/enter* client) comp loading-comp])
