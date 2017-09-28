(ns hypercrud.types.URI
  (:require [cljs.reader :as reader]))


(deftype URI [uri-str]
  Object (toString [_] (str "#URI " (pr-str uri-str)))
  IPrintWithWriter (-pr-writer [o writer _] (-write writer (.toString o)))
  IHash (-hash [this] (hash uri-str))
  IEquiv (-equiv [this other]
           (and (instance? URI other)
                (= (.-uri-str this) (.-uri-str other)))))

(def read-URI ->URI)

(reader/register-tag-parser! 'URI read-URI)

(deftype URITransitHandler []
  Object
  (tag [_ v] "r")
  (rep [_ v] (.-uri-str v))
  (stringRep [_ v] nil)
  (getVerboseHandler [_] nil))

(defn URITransitReader [v] (->URI v))
