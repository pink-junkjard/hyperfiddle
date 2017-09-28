(ns hypercrud.types.DbId
  (:require [cljs.reader :as reader]))

(deftype DbId [id uri]
  Object (toString [_] (str "#DbId" (pr-str [id uri])))
  IPrintWithWriter (-pr-writer [o writer _] (-write writer (.toString o)))
  IComparable (-compare [x y] (compare (.-id x) (.-id y)))
  IHash (-hash [this] (hash [id uri]))
  IEquiv (-equiv [this other]
           (and (instance? DbId other)
                (= (.-id this) (.-id other))
                (= (.-uri this) (.-uri other))))
  ILookup
  (-lookup [o k] (get o k nil))
  (-lookup [o k not-found] (case k
                             :id (.-id o)
                             :uri (.-uri o)
                             not-found)))

(def read-DbId #(apply ->DbId %))

(reader/register-tag-parser! 'DbId read-DbId)

(deftype DbIdTransitHandler []
  Object
  (tag [_ v] "DbId")
  (rep [_ v] [(.-id v) (.-uri v)])
  (stringRep [_ v] nil)
  (getVerboseHandler [_] nil))

(defn DbIdTransitReader [v] (apply ->DbId v))
