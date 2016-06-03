(ns hypercrud-client.core
  (:refer-clojure :exclude [update])
  (:require [hypercrud-client.util :as util]
            [hypercrud-client.hacks :as hacks]
            [goog.Uri]
            [cats.core :refer [fmap]]
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
  (query* [this named-query cmp comp])
  (transact-effect! [this named-effect txs])
  (tx [this])
  (with [this local-datoms'])
  (tempid! [this typetag]))


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


(deftype HypercrudClient [^goog.Uri entry-uri model state user-hc-dependencies force-update! local-datoms]
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
      (-> (fetch! this (goog.Uri. "/api"))
          (p/then (fn [response]
                    (let [tx (-> response :body :hypercrud :tx)]
                      (swap! state #(-> % (update-in [:tx] (constantly tx))))
                      (force-update! cmp comp)
                      tx)))                                 ;unused return value
          (p/catch (fn [error]
                     (force-update! cmp comp)
                     (p/rejected error))))))


  (entity* [this eid cmp comp]
    (.log js/console (str "Resolving entity: " eid))
    ;; if we are resolved and maybe have local edits
    ;; tempids are in the local-datoms already, probably via a not-found
    (let [tx (:tx @state)]
      (let [entity-server-datoms (get-in @state [:server-datoms [eid tx]])]
        (if (or (util/tempid? eid) (not= nil entity-server-datoms))
          (p/resolved (let [datoms-for-eid (->> (concat entity-server-datoms local-datoms) ;accounts for tx already
                                                (filter (fn [[op e a v]] (= e eid))))
                            type ((:reflect-entity-type model)
                                   (reduce (fn [acc [op e a v]]
                                             (if (= op :db/add)
                                               (assoc acc a v)
                                               (dissoc acc a)))
                                           {}
                                           datoms-for-eid)) ; todo remove this hack
                            #_(let [meta-datom (first (filter (fn [[op e a v]] (= a :meta/type)) datoms-for-eid))
                                    [op e a v] meta-datom] v)
                            form (get-in model [:forms type])
                            edited-entity (reduce (fn [acc [op e a v]]
                                                    (let [fieldinfo (first (filter #(= a (:name %)) form))]
                                                      (if (:set fieldinfo)
                                                        (if (= op :db/add)
                                                          (update-in acc [a] (fn [oldv] (if oldv (conj oldv v) #{v})))
                                                          (update-in acc [a] (fn [oldv] (if oldv (disj oldv v) #{}))))
                                                        (if (= op :db/add)
                                                          (assoc acc a v)
                                                          (dissoc acc a)))))
                                                  {}
                                                  datoms-for-eid)]
                        edited-entity))
          (let [loading (get-in @state [:pending] {})]      ;; eid -> promise
            (do
              (swap! state update-in [:cmp-deps] #(if (nil? %) #{cmp} (conj % cmp)))
              (if (contains? loading [eid tx])
                (-> (get loading [eid tx]) (p/then #(do
                                                     (swap! state update-in [:cmp-deps] disj cmp)
                                                     (force-update! cmp comp))))
                (let [promise (-> (fetch! this (goog.Uri. (str "/api/entity/" eid "?tx=" tx)))
                                  (p/then (fn [response]
                                            (let [data (-> response :body :hypercrud)]
                                              (swap! state #(-> %
                                                                (update-in [:server-datoms [eid tx]] concat (hacks/entity->datoms eid data))
                                                                (update-in [:pending] dissoc [eid tx])
                                                                (update-in [:cmp-deps] disj cmp)))
                                              (force-update! cmp comp)
                                              data)))
                                  (p/catch (fn [error]
                                             (swap! state #(-> %
                                                               (update-in [:pending] dissoc [eid tx])
                                                               (update-in [:rejected] assoc [eid tx] error)
                                                               (update-in [:cmp-deps] disj cmp)))
                                             (force-update! cmp comp)
                                             (p/rejected error))))]
                  (swap! state update-in [:pending] assoc [eid tx] promise)
                  (swap! user-hc-dependencies conj [eid tx])
                  promise))))))))


  (query* [this query cmp comp]
    {:pre [(not= nil (:tx @state))]}
    (.log js/console (str "Resolving query: " query))
    (let [tx (:tx @state)]
      (if-let [resolved-hc-query-node (get-in @state [:resolved [query tx]])]
        (p/resolved resolved-hc-query-node)
        ;; if we are resolved and maybe have local edits
        ;; tempids are in the local-datoms already, probably via a not-found
        (let [loading (get-in @state [:pending] {})]        ;; eid -> promise
          (do
            (swap! state update-in [:cmp-deps] #(if (nil? %) #{cmp} (conj % cmp)))
            (if (contains? loading [query tx])
              (-> (get loading [query tx]) (p/then #(do
                                                     (swap! state update-in [:cmp-deps] disj cmp)
                                                     (force-update! cmp comp))))
              (let [href (if (= query :index-get)
                           "/api"
                           (str "/api/query/" (name query) "?tx=" tx))
                    href (goog.Uri. href)
                    hc-node' (-> (fetch! this href)
                                 (p/then (fn [response]
                                           (let [hc-node (-> response :body :hypercrud)]
                                             (swap! state #(-> %
                                                               (update-in [:entity-types] merge (map (fn [eid] [[eid tx] (:typetag hc-node)]) (:data hc-node)))
                                                               (update-in [:templates] assoc (:typetag hc-node) (:template hc-node))
                                                               (update-in [:resolved] assoc [query tx] hc-node) ;query-resultsets
                                                               (update-in [:pending] dissoc [query tx])
                                                               (update-in [:cmp-deps] disj cmp)))
                                             (force-update! cmp comp)
                                             hc-node)))
                                 (p/catch (fn [error]
                                            (swap! state #(-> %
                                                              (update-in [:pending] dissoc [query tx])
                                                              (update-in [:rejected] assoc [query tx] error)
                                                              (update-in [:cmp-deps] disj cmp)))
                                            (force-update! cmp comp)
                                            (p/rejected error))))]
                (swap! state update-in [:pending] assoc [query tx] hc-node')
                (swap! user-hc-dependencies conj [query tx])
                hc-node')))))))


  (transact-effect! [this named-effect txs] nil)


  (tx [this]
    (get-in @state [:tx] 0))


  (with [this local-datoms']
    (HypercrudClient.
      entry-uri model state user-hc-dependencies force-update! (concat local-datoms local-datoms')))

  (tempid! [this typetag]
    ;; get the tempid from datascript
    ;; Add the :meta/type as a local-datom immediately.
    (let [tx (:tx @state)
          eid (get-in @state [:next-tempid] -1)]
      (swap! state #(-> %
                        (update-in [:next-tempid] (constantly (dec eid)))
                        (update-in [:entity-types] assoc [eid tx] typetag)))
      eid)))
