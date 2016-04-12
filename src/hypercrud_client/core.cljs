(ns hypercrud-client.core
  (:refer-clojure :exclude [update])
  (:require [hypercrud-client.util :as util]
            [goog.Uri]
            [cats.core :refer [fmap]]
            [kvlt.middleware.params]
            [kvlt.core :as kvlt]
            [promesa.core :as p]
            [reagent.core :as reagent]))



(def ^:dynamic *is-ssr* false)


(def content-type-transit "application/transit+json;charset=UTF-8")

(def transit-encoding-opts {:handlers {goog.Uri (util/UriHandler.)}})
(def transit-decoding-opts {:handlers {"r" (fn [v] (goog.Uri. v))}})


(defmethod kvlt.middleware.params/coerce-form-params (keyword content-type-transit) [{:keys [form-params]}]
  (util/transit-encode form-params :json-verbose transit-encoding-opts))


(defmethod kvlt.middleware/from-content-type (keyword content-type-transit) [resp]
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


(defn resolve [client hc-node comp & [loading-comp]]
  (let [hc-node' (resolve* client hc-node)
        cmp (reagent/current-component)]
    (if (p/resolved? hc-node')
      (comp (p/extract hc-node'))
      (do
        (if (not *is-ssr*) (p/then hc-node' #(reagent/force-update cmp)))
        (if loading-comp (loading-comp) [:div "loading"])))))


(deftype HypercrudClient [^goog.Uri entry-uri state]
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
    (kvlt/request!
      {:url     (resolve-root-relative-uri entry-uri relative-href)
       :accept  content-type-transit
       :method  :get
       :as      :auto}))

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

  (resolve* [this {:keys [href] :as cj-item}]
    (assert (not (nil? cj-item)) "resolve*: cj-item is nil")
    (assert (not (nil? href)) "resolve*: cj-item :href is nil")
    (let [cache (get-in @state [:cache] {})
          loading (get-in @state [:loading] {})] ;; map of href -> promise
      (if (contains? cache href)
        (p/resolved (get cache href))
        (if (contains? loading href)
          (get loading href)
          (let [hc-node' (->> (read this href)
                              (fmap (fn [response]
                                      (let [hc-node (-> response :body :hypercrud)
                                            cache (-> response :body :cache)
                                            cache (assoc cache href hc-node)]
                                        (assert hc-node (str "bad href: " href))
                                        (swap! state #(-> %
                                                          (update-in [:cache] merge cache)
                                                          (update-in [:loading] dissoc href)))

                                        hc-node))))]
            (swap! state update-in [:loading] assoc href hc-node')
            hc-node'
            )))))

  (enter [this comp]
    (resolve this {:href entry-uri} comp))
  )
