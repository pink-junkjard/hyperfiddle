(ns hypercrud.types.DbError
  (:gen-class)
  (:import [com.cognitect.transit WriteHandler ReadHandler]))

(deftype DbError [msg])

(defmethod print-method DbError [o ^java.io.Writer w]
  (.write w (str "#DbError" (pr-str (.msg o)))))

(def read-DbError #(->DbError %))

(deftype DbErrorTransitHandler []
  WriteHandler
  (tag [_ v] "DbError")
  (rep [_ v] (.msg v))
  (stringRep [_ v] nil)
  (getVerboseHandler [_] nil))

(deftype DbErrorTransitReader []
  ReadHandler
  (fromRep [_ v] (->DbError v)))
