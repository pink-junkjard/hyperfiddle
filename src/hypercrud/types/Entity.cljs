(ns hypercrud.types.Entity
  (:require [cljs.reader :as reader]))


(deftype Entity [dbval coll]
  Object
  (toString [o] (str "#Entity" (pr-str [dbval coll])))

  IPrintWithWriter
  (-pr-writer [o writer _] (-write writer (.toString o)))

  IIterable
  (-iterator [o] (-iterator coll))

  IEquiv
  (-equiv [this other]
    (and (instance? Entity other)
         (= (.-dbval this) (.-dbval other))
         (= (:db/id (.-coll this)) (:db/id (.-coll other)))))

  IHash
  (-hash [o] (hash [dbval (:db/id coll)]))

  ISeqable
  (-seq [o] (-seq coll))

  ICounted
  (-count [o] coll)

  ILookup
  (-lookup [o k] (-lookup coll k))
  (-lookup [o k not-found] (-lookup coll k not-found))

  IFind
  (-find [o k] (-find coll k))

  IKVReduce
  (-kv-reduce [o f init] (-kv-reduce coll f init))

  IFn
  (-invoke [o k] (-invoke coll k))
  (-invoke [o k not-found] (-invoke coll k not-found)))

(def read-Entity #(apply ->Entity %))

(reader/register-tag-parser! 'Entity read-Entity)

(deftype EntityTransitHandler []
  Object
  (tag [_ v] "Entity")
  (rep [_ v] [(.-dbval v) (.-coll v)])
  (stringRep [_ v] nil)
  (getVerboseHandler [_] nil))
