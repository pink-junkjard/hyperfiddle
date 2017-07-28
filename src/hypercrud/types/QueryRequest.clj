(ns hypercrud.types.QueryRequest
  (:import [com.cognitect.transit WriteHandler ReadHandler]))

(defrecord QueryRequest [query params pull-exps])

(deftype QueryRequestTransitHandler []
  WriteHandler
  (tag [this v] "QReq")
  (rep [this v] [(.query v) (.params v) (.pull-exps v)])
  (stringRep [this v] nil)
  (getVerboseHandler [_] nil))

(deftype QueryRequestTransitReader []
  ReadHandler
  (fromRep [_ v] (apply ->QueryRequest v)))
