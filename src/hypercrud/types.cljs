(ns hypercrud.types
  (:require [cljs.core.match :refer-macros [match]]
            [cljs.reader :as reader]
            [hypercrud.client.core :as hc]
            [hypercrud.util :as util]))


(deftype DbId [id conn-id]
  Object (toString [_] (str "#DbId" (pr-str [id conn-id])))
  IPrintWithWriter (-pr-writer [o writer _] (-write writer (.toString o)))
  IComparable (-compare [x y] (compare (.-id x) (.-id y)))
  IHash (-hash [this] (hash [id conn-id]))
  IEquiv (-equiv [this other] (= (hash this) (hash other)))
  ILookup
  (-lookup [o k] (get o k nil))
  (-lookup [o k not-found] (condp = k
                             :id (.-id o)
                             :conn-id (.-conn-id o)
                             not-found)))

(def read-DbId #(apply ->DbId %))


(deftype Entity [dbgraph dbid data ^:mutable memoize-thing]
  ILookup
  (-lookup [_ k]
    (if-let [v (get memoize-thing k)]
      v
      (let [v (get data k)
            {:keys [:db/valueType :db/cardinality]} (-> dbgraph .-schema (get k))
            v (if (and (not= nil v) (= :db.type/ref valueType))
                (condp = cardinality
                  :db.cardinality/one (hc/entity dbgraph v)
                  :db.cardinality/many (set (map #(hc/entity dbgraph %) v)))
                v)]
        (set! memoize-thing (assoc memoize-thing k v))
        v)))
  (-lookup [_ k not-found] (assert false "todo") #_(get data k not-found))

  ISeqable
  (-seq [this]
    (let [vals' (map #(get this %) (keys data))]
      (map vector (keys data) vals')))


  IHash (-hash [this] (hash [dbid data]))
  IEquiv (-equiv [this other] (= (hash this) (hash other)))
  Object (toString [_] (str "#Entity" (pr-str [dbid data])))
  IPrintWithWriter (-pr-writer [o writer _] (-write writer (.toString o))))


(deftype DbVal [conn-id t]
  Object (toString [_] (str "#DbVal" (pr-str [conn-id t])))
  IPrintWithWriter (-pr-writer [o writer _] (-write writer (.toString o)))
  IHash (-hash [this] (hash [conn-id t]))
  IEquiv (-equiv [this other] (= (hash this) (hash other)))
  ILookup
  (-lookup [o k] (get o k nil))
  (-lookup [o k not-found] (condp = k
                             :conn-id (.-conn-id o)
                             :t (.-t o)
                             not-found)))

(def read-DbVal #(apply ->DbVal %))


(reader/register-tag-parser! 'DbId read-DbId)
(reader/register-tag-parser! 'DbVal read-DbVal)


(deftype DbIdTransitHandler []
  Object
  (tag [_ v] "DbId")
  (rep [_ v] [(.-id v) (.-conn-id v)])
  (stringRep [_ v] nil)
  (getVerboseHandler [_] nil))


(defn DbIdTransitReader [v] (apply ->DbId v))


(deftype DbValTransitHandler []
  Object
  (tag [_ v] "DbVal")
  (rep [_ v] [(.-conn-id v) (.-t v)])
  (stringRep [_ v] nil)
  (getVerboseHandler [_] nil))


(defn DbValTransitReader [v] (apply ->DbVal v))
