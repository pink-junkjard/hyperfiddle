(ns hypercrud.types.ThinEntity
  (:require [cljs.reader :as reader]))


(deftype ThinEntity [dbname id]
  Object
  (toString [o] (str "#->entity" (pr-str [dbname id])))

  IPrintWithWriter
  (-pr-writer [o writer _] (-write writer (.toString o)))

  IIterable
  (-iterator [o] (-iterator {:db/id id}))

  IEquiv
  (-equiv [this other]
    (and (instance? ThinEntity other)
         (= (.-dbname this) (.-dbname other))
         (= (.-id this) (.-id other))))

  IHash
  (-hash [o] (hash [dbname id]))

  ISeqable
  (-seq [o] (-seq {:db/id id}))

  ICounted
  (-count [o] {:db/id id})

  ILookup
  (-lookup [o k] (-lookup {:db/id id} k))
  (-lookup [o k not-found] (-lookup {:db/id id} k not-found))

  IFind
  (-find [o k] (-find {:db/id id} k))

  IKVReduce
  (-kv-reduce [o f init] (-kv-reduce {:db/id id} f init))

  IFn
  (-invoke [o k] (-invoke {:db/id id} k))
  (-invoke [o k not-found] (-invoke {:db/id id} k not-found)))

(def read-ThinEntity #(apply ->ThinEntity %))

(reader/register-tag-parser! '->entity read-ThinEntity)
