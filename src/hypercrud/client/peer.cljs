(ns hypercrud.client.peer
  (:require [cats.monad.either :as either]
            [hypercrud.client.core :as hc]
            [hypercrud.client.http :as http]
            [hypercrud.types.DbVal :refer [->DbVal]]
            [hypercrud.types.DbError :refer [DbError]]
            [promesa.core :as p]
            [reagent.core :as reagent]))


(defn human-error [e req]
  (let [unfilled-holes (->> (filter (comp nil? val) (.-params req)) (map key))]
    (if-not (empty? unfilled-holes)
      [:div "Query "
       [:pre (pr-str (.-query req))]
       "has unfilled holes"
       [:pre (pr-str unfilled-holes)]
       "datomic reported"
       [:pre (.-msg e)]]
      (.-msg e))))

(defn- process-result [resultset-or-error request]
  (if resultset-or-error
    (if (instance? DbError resultset-or-error)
      (either/left (human-error resultset-or-error request))
      (either/right resultset-or-error))))

(defn hydrate-one! [entry-uri request stage-val]
  (-> (http/hydrate! entry-uri #{request} stage-val)
      (p/then (fn [{:keys [t pulled-trees-map]}]
                (process-result (get pulled-trees-map request) request)))))

(deftype Peer [state-atom]
  hc/Peer
  (hydrate [this request]
    (or (process-result @(reagent/cursor state-atom [:ptm request]) request)
        (either/left "Loading..." #_(str "Unhydrated request:\n" (pr-str request)))))

  (db [this conn-id branch]
    (->DbVal conn-id branch))

  (hydrate-one! [this request]
    (let [{:keys [entry-uri stage]} @state-atom]
      (hydrate-one! entry-uri request stage)))

  IHash
  (-hash [this] (goog/getUid this)))
