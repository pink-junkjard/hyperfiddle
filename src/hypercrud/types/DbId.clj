(ns hypercrud.types.DbId
  (:gen-class)
  (:import [com.cognitect.transit WriteHandler ReadHandler]
           [java.lang Comparable]
           [clojure.lang ILookup IHashEq]))

(deftype DbId [id conn-id]
  Comparable (compareTo [x y] (compare (.id x) (.id y)))
  IHashEq (hasheq [this] (hash [id conn-id]))
  Object (equals [this other] (= (hash this) (hash other)))
  ILookup
  (valAt [o k] (get o k nil))
  (valAt [o k not-found] (case k
                           :id (.id o)
                           :conn-id (.conn-id o)
                           not-found)))

(defmethod print-method DbId [o ^java.io.Writer w]
  (.write w (str "#DbId" (pr-str [(.id o) (.conn-id o)]))))

(defmethod print-dup DbId [o w]
  (print-method o w))

(def read-DbId #(apply ->DbId %))

(deftype DbIdTransitHandler []
  WriteHandler
  (tag [_ v] "DbId")
  (rep [_ v] [(.id v) (.conn-id v)])
  (stringRep [_ v] nil)
  (getVerboseHandler [_] nil))


(deftype DbIdTransitReader []
  ReadHandler
  (fromRep [_ v] (apply ->DbId v)))
