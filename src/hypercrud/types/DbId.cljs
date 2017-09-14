(ns hypercrud.types.DbId
  (:require [cljs.reader :as reader]))

(deftype DbId [id conn-id]
  Object (toString [_] (str "#DbId" (pr-str [id conn-id])))
  IPrintWithWriter (-pr-writer [o writer _] (-write writer (.toString o)))
  IComparable (-compare [x y] (compare (.-id x) (.-id y)))
  IHash (-hash [this] (hash [id conn-id]))
  IEquiv (-equiv [this other]
           (and (instance? DbId other)
                (= (.-id this) (.-id other))
                (= (.-conn-id this) (.-conn-id other))))
  ILookup
  (-lookup [o k] (get o k nil))
  (-lookup [o k not-found] (case k
                             :id (.-id o)
                             :conn-id (.-conn-id o)
                             not-found)))

(def read-DbId #(apply ->DbId %))

(reader/register-tag-parser! 'DbId read-DbId)

(deftype DbIdTransitHandler []
  Object
  (tag [_ v] "DbId")
  (rep [_ v] [(.-id v) (.-conn-id v)])
  (stringRep [_ v] nil)
  (getVerboseHandler [_] nil))

(defn DbIdTransitReader [v] (apply ->DbId v))
