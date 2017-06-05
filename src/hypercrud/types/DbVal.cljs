(ns hypercrud.types.DbVal
  (:require [cljs.reader :as reader]))

(deftype DbVal [conn-id branch stage-hash #_history?]
  Object (toString [_] (str "#DbVal" (pr-str [conn-id branch stage-hash])))
  IPrintWithWriter (-pr-writer [o writer _] (-write writer (.toString o)))
  IHash (-hash [this] (hash [conn-id branch stage-hash]))
  IEquiv (-equiv [this other] (= (hash this) (hash other)))
  ILookup
  (-lookup [o k] (get o k nil))
  (-lookup [o k not-found] (case k
                             :conn-id (.-conn-id o)
                             :branch (.-branch o)
                             not-found)))

(def read-DbVal #(apply ->DbVal %))

(reader/register-tag-parser! 'DbVal read-DbVal)

(deftype DbValTransitHandler []
  Object
  (tag [_ v] "DbVal")
  (rep [_ v] [(.-conn-id v) (.-branch v) (.-stage-hash v)])
  (stringRep [_ v] nil)
  (getVerboseHandler [_] nil))


(defn DbValTransitReader [v] (apply ->DbVal v))
