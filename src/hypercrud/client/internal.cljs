(ns hypercrud.client.internal
  (:require [cognitect.transit :as t]
            [goog.Uri]
            [goog.string]
            [hypercrud.types :as types]))


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
         (= (hash this) (hash other)))))                    ;TODO find a better way to check equality


(def transit-read-handlers
  {"r" (fn [v] (goog.Uri. v))
   "DbId" types/DbIdTransitReader
   "DbVal" types/DbValTransitReader
   "DbError" types/DbErrorTransitReader})

(def transit-write-handlers
  {goog.Uri (UriHandler.)
   types/DbId (types/DbIdTransitHandler.)
   types/DbVal (types/DbValTransitHandler.)
   types/DbError (types/DbErrorTransitHandler.)})


(def transit-encoding-opts {:handlers transit-write-handlers})
(def transit-decoding-opts {:handlers transit-read-handlers})


(defn transit-decode
  "Transit decode an object from `s`."
  [s & {:keys [type opts]
        :or {type :json-verbose opts transit-decoding-opts}}]
  (let [rdr (t/reader type opts)]
    (t/read rdr s)))


(defn transit-encode
  "Transit encode `x` into a String."
  [x & {:keys [type opts]
        :or {type :json-verbose opts transit-encoding-opts}}]
  (let [wrtr (t/writer type opts)]
    (t/write wrtr x)))
