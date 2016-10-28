(ns hypercrud.types
  (:require [cljs.reader :as reader]
            [hypercrud.client.core :as hc]))


(deftype DbId [id conn-id]
  ;Object (toString [this] (str "#DbId" (pr-str [id conn-id])))
  IComparable (-compare [x y] (compare (.-id x) (.-id y)))
  IPrintWithWriter (-pr-writer [_ writer _]
                     (-write writer (str "#DbId" (pr-str [id conn-id]))))
  IHash (-hash [this] (hash [id conn-id]))
  IEquiv (-equiv [this other] (= (hash this) (hash other))))

(def read-DbId #(apply ->DbId %))


(deftype Entity [dbid dbval #_"dbgraph"]
  ILookup
  (-lookup [_ k]
    (let [val (get (hc/entity dbval dbid) k)
          {:keys [:db/cardinality :db/valueType]} (get (.-schema dbval) k)]
      (if (not= nil val)
        (condp = valueType
          :db.type/ref (if (= :db.cardinality/many cardinality)
                         (set (map #(Entity. % dbval) val))
                         (Entity. val dbval))
          val))))
  (-lookup [_ k not-found] (assert false "not implemented"))

  IHash (-hash [this] (hash [dbid dbval]))
  IEquiv (-equiv [this other] (= (hash this) (hash other)))

  IPrintWithWriter (-pr-writer [_ writer _]
                     (-write writer (str "#Entity" (pr-str [dbid (hc/entity dbval dbid)])))))

(comment
  (-> (DbId. 123 :tinder) (Entity. facebook-db) :propfile/name :asdf :asdf))


(deftype DbVal [conn-id t]
  ;Object (toString [this] (str "#DbVal" (pr-str [conn-id t])))
  IPrintWithWriter (-pr-writer [_ writer _]
                     (-write writer (str "#DbVal" (pr-str [conn-id t]))))
  IHash (-hash [this] (hash [conn-id t]))
  IEquiv (-equiv [this other] (= (hash this) (hash other))))

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
