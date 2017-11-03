(ns hypercrud.client.transit
  (:require [cognitect.transit :as t]
            [hypercrud.transit :as hc-t]))


(def transit-encoding-opts {:handlers hc-t/write-handlers})
(def transit-decoding-opts {:handlers hc-t/read-handlers})

(defn decode
  "Transit decode an object from `s`."
  [s & {:keys [type opts]
        :or {type :json-verbose opts transit-decoding-opts}}]
  (let [rdr (t/reader type opts)]
    (t/read rdr s)))

(defn encode
  "Transit encode `x` into a String."
  [x & {:keys [type opts]
        :or {type :json-verbose opts transit-encoding-opts}}]
  (let [wrtr (t/writer type opts)]
    (t/write wrtr x)))
