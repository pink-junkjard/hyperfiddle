(ns hypercrud.client.core
  (:refer-clojure :exclude [update])
  (:require [cljs.core.match :refer-macros [match]]
            [goog.Uri]
            [hypercrud.client.tx-util :as tx-util]
            [hypercrud.client.util :as util]
            [kvlt.middleware.params]
            [kvlt.core :as kvlt]
            [promesa.core :as p]
            [reagent.core :as reagent]))


(def content-type-transit "application/transit+json;charset=UTF-8")


(defmethod kvlt.middleware.params/coerce-form-params (keyword content-type-transit) [{:keys [form-params]}]
  (util/transit-encode form-params))


(defmethod kvlt.middleware/from-content-type (keyword content-type-transit) [resp]
  (let [decoded-val (util/transit-decode (:body resp))]
    (assoc resp :body decoded-val)))


(defprotocol HypercrudFetch
  (fetch! [this relative-href]))


(defprotocol Hypercrud
  (enter [this cmp comp])
  (entity* [this eid cmp comp])
  (query* [this named-query cmp co88mp])
  (transact! [this txs])
  (tx [this])
  (with [this local-datoms'])
  (tempid! [this])
  (resolve-and-cache [this fetch! cmp comp key update-cache]))


(defn resolve-root-relative-uri [^goog.Uri entry-uri ^goog.Uri relative-uri]
  (-> (.clone entry-uri)
      (.resolve relative-uri)))



(defn loader [f comp & [loading-comp]]
  (let [cmp (reagent/current-component)
        v' (f cmp [loader f comp loading-comp])]
    (cond
      (p/resolved? v') (comp (p/extract v'))
      (p/rejected? v') [:div (str (.-stack (p/extract v')))]
      :pending? (if loading-comp (loading-comp) [:div "loading"]))))


(defn resolve [client eid comp & [loading-comp]]
  (loader (partial entity* client eid) comp loading-comp))


(defn resolve-query [client named-query comp & [loading-comp]]
  (loader (partial query* client named-query) comp loading-comp))


(defn resolve-enter [client comp & [loading-comp]]
  (loader (partial enter client) comp loading-comp))


(deftype HypercrudClient [^goog.Uri entry-uri schema state user-hc-dependencies force-update! local-datoms]
  HypercrudFetch
  (fetch! [this ^goog.Uri relative-href]
    (assert (not (nil? relative-href)))
    (let [start (.now js/Date)]
      (-> (kvlt/request!
            {:url (resolve-root-relative-uri entry-uri relative-href)
             :accept content-type-transit
             :method :get
             :as :auto})
          (p/finally #(do (println (str "Request took: " (- (.now js/Date) start) "ms")) %)))))


  Hypercrud
  (enter [this cmp comp]
    (if-let [tx (:tx @state)]
      (p/resolved tx)                                       ;return value tx unused?
      (-> (fetch! this entry-uri)
          (p/then (fn [response]
                    (let [tx (-> response :body :hypercrud :tx)]
                      (swap! state #(-> % (update-in [:tx] (constantly tx))))
                      (force-update! cmp comp)
                      tx)))                                 ;unused return value
          (p/catch (fn [error]
                     (force-update! cmp comp)
                     (p/rejected error))))))


  (entity* [this eid cmp comp]
    ;(.log js/console (str "Resolving entity: " eid))
    ;; if we are resolved and maybe have local edits
    ;; tempids are in the local-datoms already, probably via a not-found
    (let [tx (tx this)
          cache-key [eid tx]
          relative-href (goog.Uri. (str "/api/entity/" eid "?tx=" tx))]
      (let [entity-server-datoms (get-in @state [:server-datoms cache-key])]
        (if (or (tx-util/tempid? eid) (not= nil entity-server-datoms))
          (p/resolved (let [datoms-for-eid (->> (concat entity-server-datoms local-datoms) ;accounts for tx already
                                                (filter (fn [[op e a v]] (= e eid))))
                            edited-entity (reduce (fn [acc [op e a v]]
                                                    (let [cardinality (get-in schema [a :db/cardinality])
                                                          _ (assert cardinality (str "schema attribute not found: " (pr-str a)))]
                                                      (match [op cardinality]
                                                             [:db/add :db.cardinality/one] (assoc acc a v)
                                                             [:db/retract :db.cardinality/one] (dissoc acc a)
                                                             [:db/add :db.cardinality/many] (update-in acc [a] (fn [oldv] (if oldv (conj oldv v) #{v})))
                                                             [:db/retract :db.cardinality/many] (update-in acc [a] (fn [oldv] (if oldv (disj oldv v) #{}))))))
                                                  {}
                                                  datoms-for-eid)]
                        edited-entity))
          (resolve-and-cache this #(fetch! this relative-href)
                             cmp comp eid
                             (fn [atom data]
                               (update-in atom [:server-datoms cache-key] concat (tx-util/entity->datoms eid data))))))))


  (query* [this query cmp comp]
    ;(.log js/console (str "Resolving query: " query))
    (let [tx (tx this)
          cache-key [query tx]
          relative-href (goog.Uri. (str "/api/query?tx=" tx))]
      (if-let [query-results (get-in @state [:query-results cache-key])]
        (p/resolved query-results)
        ;; if we are resolved and maybe have local edits
        ;; tempids are in the local-datoms already, probably via a not-found
        (resolve-and-cache this #(kvlt/request!
                                  {:url (resolve-root-relative-uri entry-uri relative-href)
                                   :content-type content-type-transit
                                   :accept content-type-transit
                                   :method :post
                                   :form query
                                   :as :auto})
                           cmp comp cache-key
                           (fn [atom data]
                             (update-in atom [:query-results] assoc cache-key data))))))


  (resolve-and-cache [this fetch! cmp comp cache-key update-cache]
    (let [loading (get-in @state [:pending] {})]            ;; cache-key -> promise
      (do
        (swap! state update-in [:cmp-deps] #(if (nil? %) #{cmp} (conj % cmp)))
        (if (contains? loading cache-key)
          (-> (get loading cache-key) (p/then #(do
                                                (swap! state update-in [:cmp-deps] disj cmp)
                                                (force-update! cmp comp))))
          (let [promise (-> (fetch!)
                            (p/then (fn [response]
                                      (let [data (-> response :body :hypercrud)]
                                        (swap! state #(-> %
                                                          (update-cache data)
                                                          (update-in [:pending] dissoc cache-key)
                                                          (update-in [:cmp-deps] disj cmp)))
                                        (force-update! cmp comp)
                                        data)))
                            (p/catch (fn [error]
                                       (swap! state #(-> %
                                                         (update-in [:pending] dissoc cache-key)
                                                         (update-in [:rejected] assoc cache-key error)
                                                         (update-in [:cmp-deps] disj cmp)))
                                       (force-update! cmp comp)
                                       (p/rejected error))))]
            (swap! state update-in [:pending] assoc cache-key promise)
            (swap! user-hc-dependencies conj cache-key)
            promise)))))


  (transact! [this txs]
    (-> (kvlt/request! {:url (resolve-root-relative-uri entry-uri (goog.Uri. "/api/transact"))
                        :content-type content-type-transit
                        :accept content-type-transit
                        :method :post
                        :form txs
                        :as :auto})
        (p/then (fn [resp]
                  (swap! state update-in [:tx] (constantly (-> resp :body :hypercrud :tx)))
                  resp))))


  (tx [this]
    (get-in @state [:tx] 0))


  (with [this local-datoms']
    (HypercrudClient.
      entry-uri schema state user-hc-dependencies force-update! (concat local-datoms local-datoms')))

  (tempid! [this]
    (let [eid (get-in @state [:next-tempid] -1)]
      (swap! state update-in [:next-tempid] (constantly (dec eid)))
      eid)))
