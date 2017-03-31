(ns hypercrud.client.peer
  (:require [cats.monad.exception :as exception]
            [hypercrud.client.core :as hc]
            [hypercrud.types :as types]
            [cljs.reader :as reader]))

(defn human-error [e req]
  (let [unfilled-holes (->> (filter (comp nil? val) (.-params req)) (map key))]
    (if-not (empty? unfilled-holes)
      [:div "Query "
       [:pre (pr-str (.-query req))]
       "has unfilled holes"
       [:pre (pr-str unfilled-holes)]
       "datomic reported"
       [:pre (.-msg e)]]
      (.-msg e))))

(deftype Peer [requests pulled-trees-map]
  hc/Peer
  (hydrate [this request]
    ; (exception/try-or-recover  (constantly (exception/failure (str unfilled-holes))))
    (if-let [resultset-or-error (get pulled-trees-map request)]
      (if (instance? types/DbError resultset-or-error)
        (exception/failure (human-error resultset-or-error request))
        (exception/success resultset-or-error))
      (do
        (let [error (js/Error. (str "Unhydrated request:\n" (pr-str request)))]
          #_ (js/console.log error)                         ; happens a lot during query fns - would need to silence this log during query phase.
          (exception/failure error)))))


  (t [this]
    (hash pulled-trees-map))


  IHash
  (-hash [this]
    (hash requests))


  IEquiv
  (-equiv [this other]
    (= (hash this) (hash other)))

  ; edn readers - used in ssr
  Object (toString [_] (str "#Peer" (pr-str [requests pulled-trees-map])))
  IPrintWithWriter (-pr-writer [o writer _] (-write writer (.toString o))))


(deftype PeerTransitHandler []
  Object
  (tag [this v] "Peer")

  ; is it true that the requests are just (set (keys pulled-trees-map)) ?
  (rep [this v] [(.-requests v) (.-pulled-trees-map v)])
  (stringRep [this v] nil))

(def read-Peer #(apply ->Peer %))
(reader/register-tag-parser! 'Peer read-Peer)
