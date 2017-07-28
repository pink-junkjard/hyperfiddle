(ns hypercrud.types.EntityRequest
  (:import [com.cognitect.transit WriteHandler ReadHandler]))

(defrecord EntityRequest [e a dbval pull-exp])

(deftype EntityRequestTransitHandler []
  WriteHandler
  (tag [this v] "EReq")
  (rep [this v] [(.e v) (.a v) (.dbval v) (.pull-exp v)])
  (stringRep [this v] nil)
  (getVerboseHandler [_] nil))

(deftype EntityRequestTransitReader []
  ReadHandler
  (fromRep [_ v] (apply ->EntityRequest v)))
