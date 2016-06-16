(ns hypercrud.client.reagent-browser
  (:require [hypercrud.client.reagent :as hcr]
            [promesa.core :as p]
            [reagent.core :as reagent]))


;; The client is not used, but it is used in the SSR case for per-request state
(defmethod hcr/promised :default [client promise comp & [loading-comp]]
  (let [cmp (reagent/current-component)]
    (fn [client promise comp & [loading-comp]]
      (cond
        (p/resolved? promise) (comp (p/extract promise))
        (p/rejected? promise) [:pre (p/extract promise)]
        :pending? (do
                    ;; ignore val since hypercrud will rebuild the promise and it will be resolved next time
                    (p/then promise (fn [val] (reagent/force-update cmp)))
                    (if loading-comp (loading-comp) [:div "loading"]))))))
