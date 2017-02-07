(ns hypercrud.client.peer
  (:require [cats.monad.exception :as exception]
            [hypercrud.client.core :as hc]
            [hypercrud.types :as types]))


(deftype Peer [requests pulled-trees-map]
  hc/Peer
  (hydrate [this request]
    (if-let [resultset-or-error (get pulled-trees-map request)]
      (if (instance? types/DbError resultset-or-error)
        (exception/failure (js/Error. (.-msg resultset-or-error))) ;build a stack trace
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
