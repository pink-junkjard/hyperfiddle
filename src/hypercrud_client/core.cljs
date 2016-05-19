(ns hypercrud-client.core
  (:refer-clojure :exclude [update])
  (:require [hypercrud-client.util :as util]
            [hypercrud-client.hacks :as hacks]
            [goog.Uri]
            [cats.core :refer [fmap]]
            [kvlt.middleware.params]
            [kvlt.core :as kvlt]
            [promesa.core :as p]
            [reagent.core :as reagent]
            [datascript.core :as d]))


(def content-type-transit "application/transit+json;charset=UTF-8")


(defmethod kvlt.middleware.params/coerce-form-params (keyword content-type-transit) [{:keys [form-params]}]
  (util/transit-encode form-params))


(defmethod kvlt.middleware/from-content-type (keyword content-type-transit) [resp]
  (let [decoded-val (util/transit-decode (:body resp))]
    (assoc resp :body decoded-val)))


;; Hypercrud uses URI types, because URIs come out of transit properly typed.
(defprotocol Hypercrud
  (create [this ^goog.Uri href form])
  (read [this ^goog.Uri href])
  (update [this ^goog.Uri href form])
  (delete [this ^goog.Uri href])
  (tx [this])
  (loaded? [this hc-node])
  (is-completed? [this])
  (resolve* [this hc-node cmp comp])
  (resolve-query* [this hc-node cmp comp])
  (enter [this comp])
  (transact! [this entity])
  (with [this local-datoms'])
  (tempid [this typetag])
  (new [this template]))


(defn resolve-root-relative-uri [^goog.Uri entry-uri ^goog.Uri relative-uri]
  ;; goog.Uri.parse("//api").resolve(goog.Uri.parse("/api/communities?tx=13194139534333")).toString()
  (-> (.clone entry-uri)
      (.resolve relative-uri)
      #_.toString))


(defn resolve [client eid comp & [loading-comp]]
  (let [cmp (reagent/current-component)
        hc-node (resolve* client eid cmp [resolve client eid comp loading-comp])]
    (cond
      (p/resolved? hc-node) [comp (p/extract hc-node)]
      (p/rejected? hc-node) [:div (str (.-stack (p/extract hc-node)))]
      :pending? (if loading-comp (loading-comp) [:div "loading"]))))


(defn resolve-query [client query comp & [loading-comp]]
  (let [cmp (reagent/current-component)
        hc-node (resolve-query* client query cmp [resolve-query client query comp loading-comp])]
    (cond
      (p/resolved? hc-node) [comp (p/extract hc-node)]
      (p/rejected? hc-node) [:div (str (.-stack (p/extract hc-node)))]
      :pending? (if loading-comp (loading-comp) [:div "loading"]))))


(deftype HypercrudClient [^goog.Uri entry-uri state user-hc-dependencies force-update! local-datoms]
  Hypercrud
  (create [this ^goog.Uri relative-href form]
    (assert (not (nil? relative-href)))
    (kvlt/request!
      {:url (resolve-root-relative-uri entry-uri relative-href)
       :content-type content-type-transit
       :accept content-type-transit
       :method :post
       :form form}))

  (read [this ^goog.Uri relative-href]
    (assert (not (nil? relative-href)))
    (let [start (.now js/Date)]
      (-> (kvlt/request!
            {:url (resolve-root-relative-uri entry-uri relative-href)
             :accept content-type-transit
             :method :get
             :as :auto})
          (p/finally #(do (println (str "Request took: " (- (.now js/Date) start) "ms")) %)))))


  (update [this ^goog.Uri relative-href form]
    (assert (not (nil? relative-href)))
    (->> (kvlt/request! {:url (resolve-root-relative-uri entry-uri relative-href)
                         :content-type content-type-transit
                         :accept content-type-transit
                         :method :put
                         :form form})
         (fmap (fn [resp]
                 (swap! state update-in [:tx] (constantly (-> resp :body :tx)))
                 resp))))

  (delete [this ^goog.Uri relative-href]
    (assert (not (nil? relative-href)))
    nil)

  (tx [this]
    (get-in @state [:tx] 0))

  (loaded? [this {:keys [data] :as hc-node}]
    (not (nil? data)))

  (resolve* [this eid cmp comp]
    ;; if we are resolved and maybe have local edits
    ;; tempids are in the local-datoms already, probably via a not-found
    (let [tx (:tx @state)]
      (if-let [server-datoms-for-tx (get-in @state [:server-datoms tx])]
        (p/resolved (let [as-seen-on-server (get-in @state [:resolved [eid tx]])
                          edited-entity (-> (d/empty-db)
                                            (d/db-with server-datoms-for-tx)
                                            (d/db-with local-datoms)
                                            (d/entity eid))
                          edited-hc-node (update-in as-seen-on-server [:data] merge edited-entity)]
                      edited-hc-node))
        (let [loading (get-in @state [:pending] {})]        ;; eid -> promise
          (do
            (swap! state update-in [:cmp-deps] #(if (nil? %) #{cmp} (conj % cmp)))

            (if (contains? loading [eid tx])
              (-> (get loading [eid tx]) (p/then #(do
                                                   (swap! state update-in [:cmp-deps] disj cmp)
                                                   (force-update! cmp comp))))
              (let [entity-type (get-in @state [:entity-types [eid tx]])
                    href (str "/api/" entity-type "/" eid "?tx=" tx)
                    href (goog.Uri. href)
                    hc-node' (-> (read this href)
                                 (p/then (fn [response]
                                           (let [hc-node (-> response :body :hypercrud)]
                                             (swap! state #(-> %
                                                               (update-in [:server-datoms tx] concat (hacks/hc-node->datoms hc-node))
                                                               (update-in [:entity-types] assoc [eid tx] (hacks/hc-node->typetag hc-node))
                                                               (update-in [:templates] assoc (hacks/hc-node->typetag hc-node) (:template hc-node))

                                                               (update-in [:resolved] assoc [eid tx] hc-node)
                                                               (update-in [:pending] dissoc [eid tx])
                                                               (update-in [:cmp-deps] disj cmp)))
                                             (force-update! cmp comp)
                                             hc-node)))
                                 (p/catch (fn [error]
                                            (swap! state #(-> %
                                                              (update-in [:pending] dissoc [eid tx])
                                                              (update-in [:rejected] assoc [eid tx] error)
                                                              (update-in [:cmp-deps] disj cmp)))
                                            (force-update! cmp comp)
                                            (p/rejected error))))]
                (swap! state update-in [:pending] assoc [eid tx] hc-node')
                (swap! user-hc-dependencies conj [eid tx])
                hc-node')))))))

  (resolve-query* [this query cmp comp]
    {:pre [(not= nil (:tx @state))]}
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
                           (str "/api/" (name query) "?tx=" tx))
                    href (goog.Uri. href)
                    hc-node' (-> (read this href)
                                 (p/then (fn [response]
                                           (let [hc-node (-> response :body :hypercrud)
                                                 typetag (name query)]
                                             (swap! state #(-> %
                                                               (update-in [:entity-types] merge (map (fn [eid] [[eid tx] typetag]) (:data hc-node)))
                                                               (update-in [:templates] assoc typetag (:template hc-node))
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

  (enter [this comp]
    (resolve-query this :index-get (fn [hc-node]
                                     (if (not= (:tx @state) (:tx hc-node))
                                       (swap! state update-in [:tx] (constantly (:tx hc-node))))
                                     (comp hc-node))))

  (with [this local-datoms']
    (HypercrudClient.
      entry-uri state user-hc-dependencies force-update! (concat local-datoms local-datoms')))


  (tempid [this typetag]
    ;; get the tempid from datascript
    (let [tx (:tx @state)
          eid (get-in @state [:next-tempid] -1)]
      (swap! state #(-> %
                        (update-in [:next-tempid] dec)
                        (update-in [:entity-types] [eid tx] typetag)))
      eid))

  (new [this template]
    (let [tempid (tempid this (hacks/hc-template->typetag template))]
      {:tx (:tx @state) :rel tempid :template template :data {}})))
