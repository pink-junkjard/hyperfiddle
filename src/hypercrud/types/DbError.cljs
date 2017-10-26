(ns hypercrud.types.DbError
  (:require [cljs.reader :as reader]))

(deftype DbError [msg]
  Object (toString [_] (str "#DbError" (pr-str msg)))
  IPrintWithWriter (-pr-writer [o writer _] (-write writer (.toString o)))
  IHash (-hash [this] (hash msg))
  IEquiv (-equiv [this other]
           (and (instance? DbError other)
                (= (.-msg this) (.-msg other)))))

(def read-DbError #(->DbError %))

(reader/register-tag-parser! 'DbError read-DbError)

(deftype DbErrorTransitHandler []
  Object
  (tag [_ v] "DbError")
  (rep [_ v] (.-msg v))
  (stringRep [_ v] nil)
  (getVerboseHandler [_] nil))
