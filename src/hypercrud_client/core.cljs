(ns hypercrud-client.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:refer-clojure :exclude [update])
  (:require [goog.Uri]
            [cljs.core.async :refer [<! >! chan close! map<]]
            [cljs-http.client :as http]
            [reagent.core :as reagent]))


(deftype UriHandler []
  Object
  (tag [_ v] "r")
  (rep [_ v] (.toString v))
  (stringRep [this v] (.rep this v)))


(extend-type goog.Uri
  IHash
  (-hash [this]
    (goog.string/hashCode (pr-str this)))

  IEquiv
  (-equiv [this other]
    (and (instance? goog.Uri other)
         (= (hash this) (hash other))))) ;TODO find a better way to check equality


(def transit-opts
  {:encoding :json-verbose
   :decoding :json-verbose

   :decoding-opts
   {:handlers {"r" (fn [v] (goog.Uri. v))}}

   :encoding-opts
   {:handlers {goog.Uri (UriHandler.)}}})

(def content-type-transit "application/transit+json;charset=UTF-8")

;; Hypercrud uses URI types, because URIs come out of transit properly typed.
(defprotocol Hypercrud
  (create [this ^goog.Uri href form])
  (read [this ^goog.Uri href])
  (update [this ^goog.Uri href form])
  (delete [this ^goog.Uri href])
  (t [this])
  (loaded? [this hc-node])
  (resolve-async [this hc-node])
  (resolve-sync [this hc-node])
  (enter [this comp]))


(defn resolve-root-relative-uri [^goog.Uri entry-uri ^goog.Uri relative-uri]
  ;; goog.Uri.parse("//api").resolve(goog.Uri.parse("/api/communities?tx=13194139534333")).toString()
  (-> (.clone entry-uri)
      (.resolve relative-uri)
      #_ .toString))


(deftype HypercrudClient [^goog.Uri entry-uri cur]
  Hypercrud
  (create [this ^goog.Uri relative-href form]
    (assert (not (nil? relative-href)))
    (http/post
      (resolve-root-relative-uri entry-uri relative-href)
      {:headers {"content-type" content-type-transit
                 "accept" content-type-transit}
       :transit-params form
       :transit-opts transit-opts}))

  (read [this ^goog.Uri relative-href]
    (assert (not (nil? relative-href)))
    (http/get
      (resolve-root-relative-uri entry-uri relative-href)
      {:headers {"accept" "application/transit+json;charset=UTF-8"}
       :transit-opts transit-opts
       }))

  (update [this ^goog.Uri relative-href form]
    (assert (not (nil? relative-href)))
    (println (pr-str form))
    (->> (http/put (resolve-root-relative-uri entry-uri relative-href)
                   {:headers {"content-type" content-type-transit
                              "accept" content-type-transit}
                    :transit-params form
                    :transit-opts transit-opts})
         (map< (fn [resp]
                 (reset! (cur [:t]) (-> resp :body :t))
                 resp))))

  (delete [this ^goog.Uri relative-href]
    (assert (not (nil? relative-href)))
    nil)

  (t [this]
    @(cur [:t] 0))

  (loaded? [this {:keys [data] :as hc-node}]
    (not (nil? data)))

  (resolve-sync [this {:keys [href] :as cj-item}]
    (assert (not (nil? cj-item)) "resolve*: cj-item is nil")
    (assert (not (nil? href)) "resolve*: cj-item :href is nil")
    (let [cache @(cur [:cache])]
      (get cache href)))

  (resolve-async [this {:keys [href] :as cj-item}]
    (assert (not (nil? cj-item)) "resolve*: cj-item is nil")
    (assert (not (nil? href)) "resolve*: cj-item :href is nil")
    (let [c (chan)]
      (go
        (let [cache @(cur [:cache] {})
              cached? (contains? cache href)]
          (if cached?
            (>! c (get cache href))
            (let [resolved-cj-item-response (<! (read this href))
                  resolved-cj-item (-> resolved-cj-item-response :body :hypercrud)
                  cache (assoc (-> resolved-cj-item-response :body :cache)
                               href resolved-cj-item)]
              (assert resolved-cj-item (str "bad href: " href))
              #_ (<! (util/timeout (+ 0 (rand-int 350))))
              (swap! (cur [:cache]) merge cache)
              (>! c resolved-cj-item)))))
      c))

  (enter [this comp]
    (let [hc-node {:href entry-uri}
          a (reagent/atom (resolve-sync this hc-node))]
      (fn [this comp]
        (if (loaded? this @a)
          (comp @a)
          (do
            (go
              (let [hc-node' (<! (resolve-async this hc-node))]
                (reset! a hc-node')))
            [:div "loading"])))))
  )


(defn resolve
  "This is a reagent component, can't use it as a function. Use it like this:
      (defn comp [r] [:div r])
      [resolve cj-item comp]"
  [client cj-item comp & [loading-comp]] ;; or cj-collection, the are the same
  (let [a (reagent/atom (resolve-sync client cj-item))]
    (go
      (let [resolved-cj-item (<! (resolve-async client cj-item))]
        (reset! a resolved-cj-item)))
    (fn [client cj-item comp & [loading-comp]]
      (if (loaded? client @a)
        (comp @a)
        (if loading-comp ;for cases like :tbody that aren't allowed :div children
          (loading-comp)
          [:div "loading"])))))
