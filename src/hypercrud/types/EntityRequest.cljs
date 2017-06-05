(ns hypercrud.types.EntityRequest
  (:require [cljs.reader :as reader]))

(deftype EntityRequest [e a db pull-exp]
  Object (toString [_] (str "#EReq" (pr-str [e a db pull-exp])))
  IPrintWithWriter (-pr-writer [o writer _] (-write writer (.toString o)))
  IHash (-hash [this] (hash [e a db pull-exp]))
  IEquiv (-equiv [this other] (= (hash this) (hash other)))
  ILookup
  (-lookup [o k] (get o k nil))
  (-lookup [o k not-found] (case k
                             :e e
                             :a a
                             :db db
                             :pull-exp pull-exp
                             not-found)))

(deftype EntityRequestTransitHandler []
  Object
  (tag [this v] "EReq")
  (rep [this v] [(.-e v) (.-a v) (.-db v) (.-pull-exp v)])
  (stringRep [this v] nil))

(def read-EntityRequest #(apply ->EntityRequest %))

(reader/register-tag-parser! 'EReq read-EntityRequest)
