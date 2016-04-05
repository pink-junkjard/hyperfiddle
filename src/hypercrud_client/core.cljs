(ns hypercrud-client.core
  (:refer-clojure :exclude [update])
  (:require [hypercrud-client.util :as util]
            [goog.Uri]
            [cats.core :refer [fmap]]
            [kvlt.middleware.params]
            [kvlt.core :as kvlt]
            [promesa.core :as p]
            [reagent.core :as reagent]))


(def content-type-transit "application/transit+json;charset=UTF-8")

(def transit-encoding-opts {:handlers {goog.Uri (util/UriHandler.)}})
(def transit-decoding-opts {"r" (fn [v] (goog.Uri. v))})


(defmethod kvlt.middleware.params/coerce-form-params (keyword content-type-transit) [{:keys [form-params]}]
  (util/transit-encode form-params :json-verbose transit-encoding-opts))


(defmethod kvlt.middleware/from-content-type (keyword content-type-transit)  [resp]
  (let [decoded-val (util/transit-decode (:body resp) :json-verbose transit-decoding-opts)]
    (assoc resp :body decoded-val)))


;; Hypercrud uses URI types, because URIs come out of transit properly typed.
(defprotocol Hypercrud
  (create [this ^goog.Uri href form])
  (read [this ^goog.Uri href])
  (update [this ^goog.Uri href form])
  (delete [this ^goog.Uri href])
  (t [this])
  (loaded? [this hc-node])
  (resolve* [this hc-node])
  (enter [this comp]))


(defn resolve-root-relative-uri [^goog.Uri entry-uri ^goog.Uri relative-uri]
  ;; goog.Uri.parse("//api").resolve(goog.Uri.parse("/api/communities?tx=13194139534333")).toString()
  (-> (.clone entry-uri)
      (.resolve relative-uri)
      #_ .toString))


(defn resolve
  [client hc-node comp & [loading-comp]]
  (let [hc-node' (resolve* client hc-node)
        cmp (reagent/current-component)]
    (if (p/resolved? hc-node')
      (comp (p/extract hc-node'))
      (do
        (p/then hc-node' #(reagent/force-update cmp))
        (if loading-comp (loading-comp) [:div "loading"])))))


(deftype HypercrudClient [^goog.Uri entry-uri cur]
  Hypercrud
  (create [this ^goog.Uri relative-href form]
    (assert (not (nil? relative-href)))
    (kvlt/request!
      {:url (resolve-root-relative-uri entry-uri relative-href)
       :headers {"content-type" content-type-transit
                 "accept" content-type-transit}
       :form form}))

  (read [this ^goog.Uri relative-href]
    (assert (not (nil? relative-href)))
    (kvlt/request!
      {:url    (resolve-root-relative-uri entry-uri relative-href)
       :headers {:accept content-type-transit}
       :method :get
       :as     :auto}))

  (update [this ^goog.Uri relative-href form]
    (assert (not (nil? relative-href)))
    (println (pr-str form))
    (->> (kvlt/request! {:url (resolve-root-relative-uri entry-uri relative-href)
                         :headers {"content-type" content-type-transit
                                   "accept"       content-type-transit}
                         :form form})
         (fmap (fn [resp]
                 (reset! (cur [:t]) (-> resp :body :t))
                 resp))))

  (delete [this ^goog.Uri relative-href]
    (assert (not (nil? relative-href)))
    nil)

  (t [this]
    @(cur [:t] 0))

  (loaded? [this {:keys [data] :as hc-node}]
    (not (nil? data)))

  (resolve* [this {:keys [href] :as cj-item}]
    (assert (not (nil? cj-item)) "resolve*: cj-item is nil")
    (assert (not (nil? href)) "resolve*: cj-item :href is nil")
    (let [cache @(cur [:cache] {})
          cached? (contains? cache href)]
      (if cached?
        (p/resolved (get cache href))
        (->> (read this href)
             (fmap (fn [resolved-cj-item-response]
                    (let [resolved-cj-item (-> resolved-cj-item-response :body :hypercrud)
                          cache (-> resolved-cj-item-response :body :cache)
                          cache (assoc cache href resolved-cj-item)]
                      (assert resolved-cj-item (str "bad href: " href))
                      (swap! (cur [:cache]) merge cache)
                      resolved-cj-item)))))))

  (enter [this comp]
    (resolve this {:href entry-uri} comp))
  )
