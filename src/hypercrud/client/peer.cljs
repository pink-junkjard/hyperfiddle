(ns hypercrud.client.peer
  (:require [cats.monad.either :as either]
            [hypercrud.client.core :as hc]
            [hypercrud.client.http :as http]
            [hypercrud.types.DbVal :refer [->DbVal]]
            [hypercrud.types.DbError :refer [DbError]]
            [hypercrud.util.branch :as branch]
            [promesa.core :as p]
            [reagent.core :as reagent]))


(defn human-error [e req]
  (let [unfilled-holes (->> (filter (comp nil? val) (.-params req)) (map key))]
    (if-not (empty? unfilled-holes)
      {:message "Invalid query" :data {:datomic-error (.-msg e) :query (.-query req) :missing unfilled-holes}}
      {:message "Datomic error" :data {:datomic-error (.-msg e)}})))

(defn process-result [resultset-or-error request]
  (if (instance? DbError resultset-or-error)
    (either/left (human-error resultset-or-error request))
    (either/right resultset-or-error)))

(defn hydrate-one! [entry-uri request stage-val]
  (-> (http/hydrate! entry-uri #{request} stage-val)
      (p/then (fn [{:keys [t pulled-trees-map]}]
                (if-let [result (some-> (get pulled-trees-map request) (process-result request))]
                  (either/branch result p/rejected p/resolved)
                  (p/rejected {:message "Server failure"}))))))

(deftype Peer [state-atom]
  hc/Peer
  (hydrate [this request]
    (if-let [result @(reagent/cursor state-atom [:ptm request])]
      (process-result result request)
      (either/left {:message "Loading" :data {:request request}})))

  (db [this conn-id branch]
    (->DbVal conn-id (hash (branch/db-content conn-id branch @(reagent/cursor state-atom [:stage])))))

  (hydrate-one! [this request]
    (let [{:keys [entry-uri stage]} @state-atom]
      (hydrate-one! entry-uri request stage)))

  IHash
  (-hash [this] (goog/getUid this)))
