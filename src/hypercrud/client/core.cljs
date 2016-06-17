(ns hypercrud.client.core
  (:refer-clojure :exclude [update])
  (:require [cljs.core.match :refer-macros [match]]
            [goog.Uri]
            [hypercrud.client.fetch :as fetch]
            [hypercrud.client.tx-util :as tx-util]
            [promesa.core :as p]))


(defprotocol Hypercrud
  (enter* [this])
  (entity* [this eid])
  (query* [this query query-params])
  (transact! [this txs])
  (tx [this])
  (tx' [this])
  (with [this local-datoms'])
  (tempid! [this]))


(defprotocol HypercrudSSR
  (ssr-context [this]))


(defn resolve-and-cache* [state request-state cache-key resolve! update-cache]
  (let [loading (get-in @state [:pending] {})]              ;; cache-key -> promise
    (if (contains? loading cache-key)
      (get loading cache-key)
      (let [promise (-> (resolve!)
                        (p/then (fn [response]
                                  (let [data (-> response :body :hypercrud)]
                                    (swap! state #(-> %
                                                      (update-cache data)
                                                      (update-in [:pending] dissoc cache-key)))
                                    data)))
                        (p/catch (fn [error]
                                   (let [error (str (.-stack error))]
                                     (swap! state #(-> %
                                                       (update-in [:pending] dissoc cache-key)
                                                       (update-in [:rejected] assoc cache-key error)))
                                     (p/rejected error)))))]
        (swap! state update-in [:pending] assoc cache-key promise)
        (swap! request-state update-in [:user-hc-deps] (fn [v] (conj (or v #{}) cache-key)))
        promise))))


(deftype HypercrudClient [entry-uri schema state request-state local-datoms]
  Hypercrud
  (enter* [this]
    (let [tx (tx' this)
          cache-key [:tx tx]
          error (get-in @state [:rejected cache-key])
          query-results (get-in @state [:query-results cache-key])]
      (cond
        (not= nil error) (p/rejected error)
        (not= nil query-results) (p/resolved query-results)
        :else-request (resolve-and-cache* state request-state cache-key
                                          #(fetch/fetch! entry-uri entry-uri)
                                          (fn [old-state hc-response]
                                            (-> old-state
                                                (update-in [:initial-tx] #(or % (:tx hc-response)))
                                                (update-in [:query-results] assoc cache-key hc-response)
                                                (update-in [:tx] (constantly (:tx hc-response)))))))))


  (entity* [this eid]
    ;(.log js/console (str "Resolving entity: " eid))
    (let [tx (tx this)
          cache-key [eid tx]
          relative-href (goog.Uri. (str "/api/entity/" eid "?tx=" tx))
          error (get-in @state [:rejected cache-key])
          entity-server-datoms (get-in @state [:server-datoms cache-key])
          cached? (or (tx-util/tempid? eid) (not= nil entity-server-datoms))]
      (cond
        (not= nil error) (p/rejected error)
        cached? (p/resolved (tx-util/datoms->entity schema eid (concat entity-server-datoms local-datoms)))
        :else-request (resolve-and-cache* state request-state cache-key
                                          #(fetch/fetch! entry-uri relative-href)
                                          (fn [old-state hc-response]
                                            (update-in old-state [:server-datoms cache-key] concat (tx-util/entity->datoms eid hc-response)))))))


  (query* [this query query-params]
    ;(.log js/console (str "Resolving query: " query))
    (let [tx (tx this)
          cache-key [query query-params tx]
          relative-uri (goog.Uri. (str "/api/query?tx=" tx))
          error (get-in @state [:rejected cache-key])
          query-results (get-in @state [:query-results cache-key])]
      (cond
        (not= nil error) (p/rejected error)
        (not= nil query-results) (p/resolved query-results)
        :else-request (resolve-and-cache* state request-state cache-key
                                          #(fetch/query! entry-uri relative-uri query query-params)
                                          (fn [old-state hc-response]
                                            (update-in old-state [:query-results] assoc cache-key hc-response))))))


  (transact! [this txs]
    (-> (fetch/transact! entry-uri txs)
        (p/then (fn [resp]
                  (swap! state update-in [:tx] (constantly (-> resp :body :hypercrud :tx)))
                  resp))))


  (tx [this]
    (get-in @state [:tx]))


  (tx' [this]
    (let [tx (get-in @state [:tx] 0)
          initial-tx (get-in @state [:initial-tx] 0)]
      (if (= tx initial-tx)
        :initial-tx
        tx)))


  (with [this local-datoms']
    (HypercrudClient.
      entry-uri schema state request-state (concat local-datoms local-datoms')))

  (tempid! [this]
    (let [eid (get-in @state [:next-tempid] -1)]
      (swap! state update-in [:next-tempid] (constantly (dec eid)))
      eid))

  HypercrudSSR
  (ssr-context [this] request-state))
