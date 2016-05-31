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
  (with [this local-datoms'])
  (tempid! [this typetag])
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
      (p/resolved? hc-node) (comp (p/extract hc-node))
      (p/rejected? hc-node) [:div (str (.-stack (p/extract hc-node)))]
      :pending? (if loading-comp (loading-comp) [:div "loading"]))))


(defn resolve-query [client query comp & [loading-comp]]
  (let [cmp (reagent/current-component)
        hc-node (resolve-query* client query cmp [resolve-query client query comp loading-comp])]
    (cond
      (p/resolved? hc-node) (comp (p/extract hc-node))
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
    (.log js/console (str "Resolving entity: " eid))
    ;; if we are resolved and maybe have local edits
    ;; tempids are in the local-datoms already, probably via a not-found
    (let [tx (:tx @state)]
      (let [entity-server-datoms (get-in @state [:server-datoms [eid tx]])]
        (if (or (util/tempid? eid) (not= nil entity-server-datoms))
          (p/resolved (let [entity-type (get-in @state [:entity-types [eid tx]])
                            template (get-in @state [:templates entity-type])
                            _ (assert template (str "No template found for " eid))
                            as-seen-on-server (or (get-in @state [:resolved [eid tx]])
                                                  {:tx tx
                                                   :rel eid
                                                   :links []
                                                   :type :object
                                                   :data {}
                                                   :queries []
                                                   :typetag entity-type
                                                   :template template})

                            edited-entity (->> (concat entity-server-datoms local-datoms) ;accounts for tx already
                                               (filter (fn [[op e a v]] (= e eid)))
                                               (reduce (fn [acc [op e a v]]
                                                         (let [fieldinfo (first (filter #(= a (:name %)) (:data template)))]
                                                           (if (:set fieldinfo)
                                                             (if (= op :db/add)
                                                               (update-in acc [a] (fn [oldv] (if oldv (conj oldv v) #{v})))
                                                               (update-in acc [a] (fn [oldv] (if oldv (disj oldv v) #{}))))
                                                             (if (= op :db/add)
                                                               (assoc acc a v)
                                                               (dissoc acc a)))))
                                                       {}))
                            edited-hc-node (update-in as-seen-on-server [:data] (constantly edited-entity))]
                        edited-hc-node))
          (let [loading (get-in @state [:pending] {})]      ;; eid -> promise
            (do
              (swap! state update-in [:cmp-deps] #(if (nil? %) #{cmp} (conj % cmp)))

              (if (contains? loading [eid tx])
                (-> (get loading [eid tx]) (p/then #(do
                                                     (swap! state update-in [:cmp-deps] disj cmp)
                                                     (force-update! cmp comp))))
                (let [entity-type (get-in @state [:entity-types [eid tx]])
                      _ (assert entity-type (str "No entity type found for " eid))
                      href (str "/api/" (hacks/typetag->query entity-type) "/" eid "?tx=" tx)
                      href (goog.Uri. href)
                      hc-node' (-> (read this href)
                                   (p/then (fn [response]
                                             (let [hc-node (-> response :body :hypercrud)]
                                               (swap! state #(-> %
                                                                 (update-in [:server-datoms [eid tx]] concat (hacks/hc-node->datoms hc-node))
                                                                 (update-in [:entity-types] assoc [eid tx] (:typetag hc-node))
                                                                 (update-in [:entity-types] merge
                                                                            (->> (:template hc-node)
                                                                                 (:data)
                                                                                 (filter (fn [t] (= (:datatype t) :ref)))
                                                                                 (map (fn [t] (:name t)))
                                                                                 (select-keys (:data hc-node))
                                                                                 (map (fn [[k v]]
                                                                                        (let [typeinfo (first (filter (fn [t] (= k (:name t)))
                                                                                                                      (get-in hc-node [:template :data])))]
                                                                                          (if (:set typeinfo)
                                                                                            (map (fn [v]
                                                                                                   [[v tx] (:typetag typeinfo)])
                                                                                                 v)
                                                                                            [[[v tx] (:typetag typeinfo)]]))))
                                                                                 (apply concat)))
                                                                 (update-in [:templates] assoc (:typetag hc-node) (:template hc-node))

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
                  hc-node'))))))))

  (resolve-query* [this query cmp comp]
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
                           (str "/api/" (name query) "?tx=" tx))
                    href (goog.Uri. href)
                    hc-node' (-> (read this href)
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

  (enter [this comp]
    (resolve-query this :index-get (fn [hc-node]
                                     (if (not= (:tx @state) (:tx hc-node))
                                       (swap! state update-in [:tx] (constantly (:tx hc-node))))
                                     (comp hc-node))))

  (with [this local-datoms']
    (HypercrudClient.
      entry-uri state user-hc-dependencies force-update! (concat local-datoms local-datoms')))


  (tempid! [this typetag]
    ;; get the tempid from datascript
    (let [tx (:tx @state)
          eid (get-in @state [:next-tempid] -1)]
      (swap! state #(-> %
                        (update-in [:next-tempid] (constantly (dec eid)))
                        (update-in [:entity-types] assoc [eid tx] typetag)))
      eid)))
