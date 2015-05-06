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


(def transit-opts
  {:encoding :json-verbose
   :decoding :json-verbose

   :decoding-opts
             {:handlers {"r" (fn [v] (goog.Uri. v))}}

   :encoding-opts
             {:handlers {goog.Uri (UriHandler.)}}})

(def content-type-transit "application/transit+json;charset=UTF-8")


(defprotocol Hypercrud
  (create [this href form])
  (read [this href])
  (update [this href form])
  (delete [this href])
  (t [this])
  (loaded? [this hc-node])
  (resolve* [this hc-node]))


(defn href-with-base [^String base ^goog.Uri href]
  (goog.Uri. (str base (.toString href))))


(deftype HypercrudClient [base-href cur]
  Hypercrud
  (create [this ^goog.Uri href form]
    (assert (not (nil? href)))
    (http/post
      (href-with-base base-href href)
      {:headers {"content-type" content-type-transit
                 "accept" content-type-transit}
       :transit-params form
       :transit-opts transit-opts}))

  (read [this ^goog.Uri href]
    (assert (not (nil? href)))
    (http/get
      (href-with-base base-href href)
      {:headers {"accept" "application/transit+json;charset=UTF-8"}
       :transit-opts transit-opts
       }))

  (update [this ^goog.Uri href form]
    (assert (not (nil? href)))
    (println (pr-str form))
    (->> (http/put (href-with-base base-href href)
                   {:headers {"content-type" content-type-transit
                              "accept" content-type-transit}
                    :transit-params form
                    :transit-opts transit-opts})
         (map< (fn [resp]
                 (reset! (cur [:t]) (-> resp :body :t))
                 resp))))

  (delete [this ^goog.Uri href]
    (assert (not (nil? href)))
    nil)

  (t [this]
    @(cur [:t]))

  (loaded? [this {:keys [data] :as hc-node}]
    (not (nil? data)))

  (resolve* [this cj-item]
    (assert (not (nil? cj-item)) "resolve*: cj-item is nil")
    (assert (not (nil? (:href cj-item))) "resolve*: cj-item :href is nil")
    (let [c (chan)]
      (go
        (let [resolved-cj-item-response (<! (read this (:href cj-item)))
              resolved-cj-item (-> resolved-cj-item-response :body :hypercrud)
              cache (-> resolved-cj-item-response :body :cache)]
          (assert resolved-cj-item (str "bad href: " (:href cj-item)))
          #_ (<! (util/timeout (+ 0 (rand-int 350))))
          (reset! (cur [:cache]) cache)                     ;merge?
          (>! c resolved-cj-item)))
      c))
  )


(defn resolve
  "This is a reagent component, can't use it as a function. Use it like this:
      (defn comp [r] [:div r])
      [resolve cj-item comp]"
  [client cj-item comp] ;; or cj-collection, the are the same
  (let [a (reagent/atom cj-item)]
    (go
      (if (loaded? client cj-item)
        (reset! a cj-item) ;skip request
        (let [resolved-cj-item (<! (resolve* client cj-item))]
          ;;(println "resolved" resolved-cj-item)
          (reset! a resolved-cj-item))))
    (fn [client cj-item comp]
      (comp @a))))
