(ns hypercrud.client.peer
  (:require [cats.monad.either :as either]
            [hypercrud.client.core :as hc]
            [hypercrud.client.http :as http]
            [hypercrud.types.DbVal :refer [->DbVal]]
            [hypercrud.types.Err :refer [Err]]
            [hypercrud.util.branch :as branch]
            [promesa.core :as p]
            [reagent.core :as reagent]))


(defn human-error [e req]
  (let [unfilled-holes (->> (filter (comp nil? val) (.-params req)) (map key))]
    (if-not (empty? unfilled-holes)
      {:message "Invalid query" :data {:datomic-error (.-msg e) :query (.-query req) :missing unfilled-holes}}
      {:message "Datomic error" :data {:datomic-error (.-msg e)}})))

(defn process-result [resultset-or-error request]
  (if (instance? Err resultset-or-error)
    (either/left (human-error resultset-or-error request))
    (either/right resultset-or-error)))

(defn hydrate-one! [service-uri request stage-val]
  (-> (http/hydrate! service-uri #{request} stage-val)
      (p/then (fn [{:keys [t pulled-trees-map id->tempid]}]
                (if-let [result (some-> (get pulled-trees-map request) (process-result request))]
                  (either/branch result p/rejected p/resolved)
                  (p/rejected {:message "Server failure"}))))))

(deftype Peer [state-atom]
  hc/Peer
  (hydrate [this request]
    (if-let [result @(reagent/cursor state-atom [:ptm request])]
      (process-result result request)
      (either/left {:message "Loading" :data {:request request}})))

  (db [this uri branch]
    (->DbVal uri (branch/branch-val uri branch @(reagent/cursor state-atom [:stage]))))

  (hydrate-one! [this service-uri request]
    (let [{:keys [stage]} @state-atom]
      (hydrate-one! service-uri request stage)))

  IHash
  (-hash [this] (goog/getUid this)))
