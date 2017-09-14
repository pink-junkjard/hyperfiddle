(ns hypercrud.types.DbVal
  (:require [cljs.reader :as reader]))

(deftype DbVal [conn-id branch #_history?]
  Object (toString [_] (str "#DbVal" (pr-str [conn-id branch])))
  IPrintWithWriter (-pr-writer [o writer _] (-write writer (.toString o)))
  IHash (-hash [this] (hash [conn-id branch]))
  IEquiv (-equiv [this other]
           (and (instance? DbVal other)
                (= (.-conn-id this) (.-conn-id other))
                (= (.-branch this) (.-branch other))))
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
  (rep [_ v] [(.-conn-id v) (.-branch v)])
  (stringRep [_ v] nil)
  (getVerboseHandler [_] nil))


(defn DbValTransitReader [v] (apply ->DbVal v))
