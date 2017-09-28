(ns hypercrud.types.DbId
  (:import [com.cognitect.transit WriteHandler ReadHandler]
           [java.lang Comparable]
           [clojure.lang ILookup IHashEq]))

(deftype DbId [id uri]
  Comparable (compareTo [x y] (compare (.id x) (.id y)))
  IHashEq (hasheq [this] (hash [id uri]))
  Object (equals [this other]
           (and (instance? DbId other)
                (= (.id this) (.id other))
                (= (.uri this) (.uri other))))
  ILookup
  (valAt [o k] (get o k nil))
  (valAt [o k not-found] (case k
                           :id (.id o)
                           :uri (.uri o)
                           not-found)))

(defmethod print-method DbId [o ^java.io.Writer w]
  (.write w (str "#DbId" (pr-str [(.id o) (.uri o)]))))

(defmethod print-dup DbId [o w]
  (print-method o w))

(def read-DbId #(apply ->DbId %))

(deftype DbIdTransitHandler []
  WriteHandler
  (tag [_ v] "DbId")
  (rep [_ v] [(.id v) (.uri v)])
  (stringRep [_ v] nil)
  (getVerboseHandler [_] nil))


(deftype DbIdTransitReader []
  ReadHandler
  (fromRep [_ v] (apply ->DbId v)))
