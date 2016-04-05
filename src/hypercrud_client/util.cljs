(ns hypercrud-client.util
  (:require [goog.Uri]
            [cognitect.transit :as t]))


(defn transit-decode
  "Transit decode an object from `s`."
  [s type opts]
  (let [rdr (t/reader type opts)]
    (t/read rdr s)))


(defn transit-encode
  "Transit encode `x` into a String."
  [x type opts]
  (let [wrtr (t/writer type opts)]
    (t/write wrtr x)))


;; transit uri encoder type
(deftype UriHandler []
  Object
  (tag [_ v] "r")
  (rep [_ v] (.toString v))
  (stringRep [this v] (.rep this v)))


;; allow goog.Uri as key in clojure map
(extend-type goog.Uri
  IHash
  (-hash [this]
    (goog.string/hashCode (pr-str this)))

  IEquiv
  (-equiv [this other]
    (and (instance? goog.Uri other)
         (= (hash this) (hash other))))) ;TODO find a better way to check equality
