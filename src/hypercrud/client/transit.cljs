(ns hypercrud.client.transit
  (:require [cognitect.transit :as t]
            [hypercrud.transit :as hc-t]
            [hypercrud.types.Entity :refer [Entity ThinEntity read-Entity read-ThinEntity]]
            [hypercrud.types.URI :refer [URI read-URI]]))


(def transit-read-handlers
  (merge hc-t/read-handlers
         {"Entity" (t/read-handler read-Entity)
          "->entity" (t/read-handler read-ThinEntity)
          "r" (t/read-handler read-URI)}))

(def transit-write-handlers
  (merge hc-t/write-handlers
         {URI
          (t/write-handler (constantly "r") (fn [v] (.-uri-str v)))

          Entity
          (t/write-handler (constantly "Entity") (fn [v] [(.-dbval v) (.-coll v)]))

          ThinEntity
          (t/write-handler (constantly "->entity") (fn [v] [(.-dbname v) (.-id v)]))}))

(def transit-encoding-opts {:handlers transit-write-handlers})
(def transit-decoding-opts {:handlers transit-read-handlers})

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
