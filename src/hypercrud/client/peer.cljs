(ns hypercrud.client.peer
  (:require [cats.monad.exception :as exception]
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

(defn- hydrate [resultset-or-error request]
  (if resultset-or-error
    (if (instance? DbError resultset-or-error)
      (exception/failure (human-error resultset-or-error request))
      (exception/success resultset-or-error))))

(defn hydrate-one! [entry-uri request stage-val]
  (-> (http/hydrate! entry-uri #{request} stage-val)
      (p/then (fn [{:keys [t pulled-trees-map]}]
                (hydrate (get pulled-trees-map request) request)))))

(def loading-response (exception/success [:div "loading"]))

(deftype Peer [state-atom watches]
  hc/Peer
  (hydrate [this request]
    (or (hydrate @(reagent/cursor state-atom [:ptm request]) request)
        (exception/failure (js/Error. (str "Unhydrated request:\n" (pr-str request)))))

    #_(let [{:keys [hydrate-id ptm]} @state-atom]
      (if-let [result (hydrate ptm request)]
        result
        (if (and hydrate-id *reactive-hydrate?*)
          loading-response
          (exception/failure (js/Error. (str "Unhydrated request:\n" (pr-str request))))))))

  (db [this conn-id branch]
    ; todo fix dbval type, remove stage-hash
    (->DbVal conn-id branch nil))

  (hydrate-one! [this request]
    (let [{:keys [entry-uri stage]} @state-atom]
      (hydrate-one! entry-uri request stage)))

  IWatchable
  (-notify-watches [this oldval newval]
    (doseq [[key f] watches]
      (f key this oldval newval)))
  (-add-watch [this key f]
    (set! (.-watches this) (conj watches [key f]))
    this)
  (-remove-watch [this key]
    (set! (.-watches this) (apply vector (remove #(= key (first %)) watches))))

  IHash
  (-hash [this] (goog/getUid this)))

(defn ->peer [state-atom]
  (let [peer (->Peer state-atom [])]
    (add-watch state-atom (hash peer)
               (fn [k r o n]
                 (let [o (select-keys o [:ptm :stage])
                       n (select-keys n [:ptm :stage])]
                   #_(when (not= o n))
                   (-notify-watches peer o n))))
    peer))