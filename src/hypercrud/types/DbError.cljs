(ns hypercrud.types.DbError
  (:require [cljs.reader :as reader]))

(deftype DbError [msg]
  Object (toString [_] (str "#DbError" (pr-str msg)))
  IPrintWithWriter (-pr-writer [o writer _] (-write writer (.toString o))))

(def read-DbError #(->DbError %))

(reader/register-tag-parser! 'DbError read-DbError)

(deftype DbErrorTransitHandler []
  Object
  (tag [_ v] "DbError")
  (rep [_ v] (.-msg v))
  (stringRep [_ v] nil)
  (getVerboseHandler [_] nil))

(defn DbErrorTransitReader [v] (->DbError v))
