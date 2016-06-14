(ns hypercrud.client.core
  (:refer-clojure :exclude [update])
  (:require [cljs.core.match :refer-macros [match]]
            [goog.Uri]
            [hypercrud.client.fetch :as fetch]
            [hypercrud.client.tx-util :as tx-util]
            [promesa.core :as p]))


(defprotocol Hypercrud
  (enter* [this cmp comp])
  (entity* [this eid cmp comp])
  (query* [this named-query cmp comp])
  (transact! [this txs])
  (tx [this])
  (with [this local-datoms'])
  (tempid! [this]))


(defn resolve-and-cache* [state user-hc-dependencies force-update! cmp comp cache-key resolve! update-cache]
  (let [loading (get-in @state [:pending] {})]              ;; cache-key -> promise
    (do
      (swap! state update-in [:cmp-deps] #(if (nil? %) #{cmp} (conj % cmp)))
      (if (contains? loading cache-key)
        (-> (get loading cache-key) (p/then #(do
                                              (swap! state update-in [:cmp-deps] disj cmp)
                                              (force-update! cmp comp))))
        (let [promise (-> (resolve!)
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


(deftype HypercrudClient [entry-uri schema state user-hc-dependencies force-update! local-datoms]
  Hypercrud
  (enter* [this cmp comp]
    (let [tx (tx this)
          cache-key [:tx tx]]
      (if (not= 0 tx)
        (p/resolved tx)
        (resolve-and-cache* state user-hc-dependencies force-update! cmp comp cache-key
                            #(fetch/fetch! entry-uri entry-uri)
                            (fn [old-state hc-response]
                              (-> old-state
                                  (update-in [:query-results] assoc cache-key hc-response)
                                  (update-in [:tx] (constantly (:tx hc-response)))))))))


  (entity* [this eid cmp comp]
    ;(.log js/console (str "Resolving entity: " eid))
    (let [tx (tx this)
          cache-key [eid tx]
          relative-href (goog.Uri. (str "/api/entity/" eid "?tx=" tx))
          entity-server-datoms (get-in @state [:server-datoms cache-key])]
      (if (or (tx-util/tempid? eid) (not= nil entity-server-datoms))
        (p/resolved (tx-util/datoms->entity schema eid (concat entity-server-datoms local-datoms)))
        (resolve-and-cache* state user-hc-dependencies force-update! cmp comp cache-key
                            #(fetch/fetch! entry-uri relative-href)
                            (fn [old-state hc-response]
                              (update-in old-state [:server-datoms cache-key] concat (tx-util/entity->datoms eid hc-response)))))))


  (query* [this query cmp comp]
    ;(.log js/console (str "Resolving query: " query))
    (let [tx (tx this)
          cache-key [query tx]
          relative-uri (goog.Uri. (str "/api/query?tx=" tx))]
      (if-let [query-results (get-in @state [:query-results cache-key])]
        (p/resolved query-results)
        (resolve-and-cache* state user-hc-dependencies force-update! cmp comp cache-key
                            #(fetch/query! entry-uri relative-uri query)
                            (fn [old-state hc-response]
                              (update-in old-state [:query-results] assoc cache-key hc-response))))))


  (transact! [this txs]
    (-> (fetch/transact! entry-uri txs)
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
