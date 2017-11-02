(ns hypercrud.client.transit
  (:require [cognitect.transit :as t]
            [hypercrud.transit :as hc-t]
            [hypercrud.types.DbVal :refer [DbVal read-DbVal]]
            [hypercrud.types.Entity :refer [Entity ThinEntity read-Entity read-ThinEntity]]
            [hypercrud.types.QueryRequest :refer [QueryRequest read-QueryRequest]]
            [hypercrud.types.EntityRequest :refer [EntityRequest read-EntityRequest]]
            [hypercrud.types.URI :refer [URI read-URI]]))


(def transit-read-handlers
  (merge hc-t/read-handlers
         {"DbVal" (t/read-handler read-DbVal)
          "Entity" (t/read-handler read-Entity)
          "->entity" (t/read-handler read-ThinEntity)
          "EReq" (t/read-handler read-EntityRequest)
          "QReq" (t/read-handler read-QueryRequest)
          "r" (t/read-handler read-URI)}))

(def transit-write-handlers
  (merge hc-t/write-handlers
         {URI
          (t/write-handler (constantly "r") (fn [v] (.-uri-str v)))

          DbVal
          (t/write-handler (constantly "DbVal") (fn [v] [(.-uri v) (.-branch v)]))

          Entity
          (t/write-handler (constantly "Entity") (fn [v] [(.-dbval v) (.-coll v)]))

          ThinEntity
          (t/write-handler (constantly "->entity") (fn [v] [(.-dbname v) (.-id v)]))

          QueryRequest
          (t/write-handler (constantly "QReq") (fn [v] [(.-query v) (.-params v) (.-pull-exps v)]))

          EntityRequest
          (t/write-handler (constantly "EReq") (fn [v] [(.-e v) (.-a v) (.-db v) (.-pull-exp v)]))}))

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
