(ns hypercrud.types.DbVal
  (:require [cljs.reader :as reader]))

(deftype DbVal [uri branch #_history?]
  Object (toString [_] (str "#DbVal" (pr-str [uri branch])))
  IPrintWithWriter (-pr-writer [o writer _] (-write writer (.toString o)))
  IHash (-hash [this] (hash [uri branch]))
  IEquiv (-equiv [this other]
           (and (instance? DbVal other)
                (= (.-uri this) (.-uri other))
                (= (.-branch this) (.-branch other))))
  ILookup
  (-lookup [o k] (get o k nil))
  (-lookup [o k not-found] (case k
                             :uri (.-uri o)
                             :branch (.-branch o)
                             not-found)))

(def read-DbVal #(apply ->DbVal %))

(reader/register-tag-parser! 'DbVal read-DbVal)
