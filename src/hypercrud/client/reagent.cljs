(ns hypercrud.client.reagent
  (:require [hypercrud.client.core :as hc]
            [promesa.core :as p]
            [reagent.core :as reagent]))


(defmulti force-update! (fn [cmp comp-fn] :default))


(defn- then-force-update! [promise client comp]
  (let [cmp (reagent/current-component)]                    ;happens in reagent stack frame
    (swap! (hc/ssr-context client) update-in [:cmp-deps] (fn [v] (inc (or v 0))))
    (p/then promise
            (fn [val]
              (force-update! cmp #(comp val))
              (swap! (hc/ssr-context client) update-in [:cmp-deps] dec)
              val))))


(defn promised [client promise comp & [loading-comp]]
  (cond
    (p/resolved? promise) (comp (p/extract promise))
    (p/rejected? promise) [:pre (p/extract promise)]
    (p/pending? promise) (let [_ (then-force-update! promise client comp)]
                           (if loading-comp (loading-comp) [:div "loading"]))))


(defn entity [client eid comp & [loading-comp]]
  [promised client (hc/entity* client eid) comp loading-comp])


(defn query
  ([client q comp] (query client q [] comp))
  ([client q query-params comp & [loading-comp]]
   [promised client (hc/query* client q query-params) comp loading-comp]))


(defn enter [client comp & [loading-comp]]
  ^{:key (hc/tx' client)}
  [promised client (hc/enter* client) comp loading-comp])
