(ns hypercrud.types.QueryRequest
  (:require [cljs.reader :as reader]))

(deftype QueryRequest [query params pull-exps]
  Object (toString [_] (str "#QReq" (pr-str [query params pull-exps])))
  IPrintWithWriter (-pr-writer [o writer _] (-write writer (.toString o)))
  IHash (-hash [this] (hash [query params pull-exps]))
  IEquiv (-equiv [this other] (= (hash this) (hash other)))
  ILookup
  (-lookup [o k] (get o k nil))
  (-lookup [o k not-found] (case k
                             :query query
                             :params params
                             :pull-exps pull-exps
                             not-found)))

(deftype QueryRequestTransitHandler []
  Object
  (tag [this v] "QReq")
  (rep [this v] [(.-query v) (.-params v) (.-pull-exps v)])
  (stringRep [this v] nil))

(def read-QueryRequest #(apply ->QueryRequest %))

(reader/register-tag-parser! 'QReq read-QueryRequest)
