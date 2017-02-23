(ns hypercrud.client.peer
  (:require [cats.monad.exception :as exception]
            [hypercrud.client.core :as hc]
            [hypercrud.types :as types]))


(defn human-error [e req]
  (let [unfilled-holes (->> (filter (comp nil? val) (.-params req)) (map key))]
    (if-not (empty? unfilled-holes)
      (pr-str unfilled-holes)
      e)))


(deftype Peer [requests pulled-trees-map]
  hc/Peer
  (hydrate [this request]
    ; (exception/try-or-recover  (constantly (exception/failure (str unfilled-holes))))


    (if-let [resultset-or-error (get pulled-trees-map request)]
      (if (instance? types/DbError resultset-or-error)
        (exception/failure (human-error resultset-or-error request))
        (exception/success resultset-or-error))
      (exception/failure (js/Error. (str "Unhydrated request:\n" (pr-str request))))))


  (t [this]
    (hash pulled-trees-map))


  IHash
  (-hash [this]
    (hash requests))


  IEquiv
  (-equiv [this other]
    (= (hash this) (hash other))))
