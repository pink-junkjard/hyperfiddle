(ns hypercrud-client.core
  (:refer-clojure :exclude [update])
  (:require [hypercrud-client.util :as util]
            [clojure.set :as set]
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
  (t [this])
  (loaded? [this hc-node])
  (is-completed? [this])
  (force-update [this cmp comp])
  (html-ready! [this])
  (resolve* [this hc-node])
  (enter [this comp]))


(defn resolve-root-relative-uri [^goog.Uri entry-uri ^goog.Uri relative-uri]
  ;; goog.Uri.parse("//api").resolve(goog.Uri.parse("/api/communities?tx=13194139534333")).toString()
  (-> (.clone entry-uri)
      (.resolve relative-uri)
      #_.toString))


(defn resolve [client hc-node comp & [loading-comp]]
  (let [hc-node' (resolve* client hc-node)
        cmp (reagent/current-component)]
    (cond
      (p/resolved? hc-node') (do
                               (.setTimeout js/global #(if (is-completed? client) (html-ready! client)) 0)
                               (comp (p/extract hc-node')))

      (p/rejected? hc-node') [:div (str (.-stack (p/extract hc-node')))]
      :pending? (do
                  (p/finally hc-node' #(force-update client cmp [resolve client hc-node comp loading-comp]))
                  (if loading-comp (loading-comp) [:div "loading"])))))


(deftype HypercrudClient [^goog.Uri entry-uri state user-hc-dependencies force-update* html-ready!*]
  Hypercrud
  (create [this ^goog.Uri relative-href form]
    (assert (not (nil? relative-href)))
    (kvlt/request!
      {:url          (resolve-root-relative-uri entry-uri relative-href)
       :content-type content-type-transit
       :accept       content-type-transit
       :method       :post
       :form         form}))

  (read [this ^goog.Uri relative-href]
    (assert (not (nil? relative-href)))
    (let [start (.now js/Date)]
      (-> (kvlt/request!
            {:url    (resolve-root-relative-uri entry-uri relative-href)
             :accept content-type-transit
             :method :get
             :as     :auto})
          (p/finally #(do (println (str "Request took: " (- (.now js/Date) start) "ms")) %)))))


  (update [this ^goog.Uri relative-href form]
    (assert (not (nil? relative-href)))
    (js/alert (pr-str form))
    (->> (kvlt/request! {:url          (resolve-root-relative-uri entry-uri relative-href)
                         :content-type content-type-transit
                         :accept       content-type-transit
                         :method       :put
                         :form         form})
         (fmap (fn [resp]
                 (swap! state update-in [:t] (constantly (-> resp :body :t)))
                 resp))))

  (delete [this ^goog.Uri relative-href]
    (assert (not (nil? relative-href)))
    nil)

  (t [this]
    (get-in @state [:t] 0))

  (loaded? [this {:keys [data] :as hc-node}]
    (not (nil? data)))

  (is-completed? [this]
    (set/subset? @user-hc-dependencies (into #{} (-> @state :resolved keys)))) ; todo rejected

  (force-update [this cmp comp]
    (force-update* cmp comp))

  (html-ready! [this]
    (html-ready!*))

  (resolve* [this {:keys [href] :as cj-item}]
    (assert (not (nil? cj-item)) "resolve*: cj-item is nil")
    (assert (not (nil? href)) "resolve*: cj-item :href is nil")
    (let [resolved (get-in @state [:resolved] {})
          loading (get-in @state [:pending] {})]            ;; map of href -> promise
      (if (contains? resolved href)
        (p/resolved (get resolved href))
        (if (contains? loading href)
          (get loading href)
          (let [hc-node' (-> (read this href)
                             (p/then (fn [response]
                                       (let [hc-node (-> response :body :hypercrud)]
                                         (assert hc-node (str "bad href: " href))
                                         (swap! state #(-> %
                                                           (update-in [:resolved] assoc href hc-node)
                                                           (update-in [:resolved] merge (-> response :body :cache))
                                                           (update-in [:pending] dissoc href)))
                                         hc-node)))
                             (p/catch (fn [error]
                                        (swap! state #(-> %
                                                          (update-in [:pending] dissoc href)
                                                          (update-in [:rejected] assoc href error)))
                                        (p/rejected error))))]

            (swap! state update-in [:pending] assoc href hc-node')
            (swap! user-hc-dependencies conj href)
            hc-node')))))


  (enter [this comp]
    (resolve this {:href entry-uri} comp)))
