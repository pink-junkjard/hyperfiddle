(ns hypercrud.client.transit
  (:require [cognitect.transit :as t]
            [hypercrud.types.DbError :refer [DbError read-DbError DbErrorTransitHandler]]
            [hypercrud.types.DbVal :refer [DbVal read-DbVal DbValTransitHandler]]
            [hypercrud.types.Entity :refer [Entity EntityTransitHandler ThinEntity ThinEntityTransitHandler read-Entity read-ThinEntity]]
            [hypercrud.types.QueryRequest :refer [QueryRequest read-QueryRequest QueryRequestTransitHandler]]
            [hypercrud.types.EntityRequest :refer [EntityRequest read-EntityRequest EntityRequestTransitHandler]]
            [hypercrud.types.URI :refer [URI read-URI URITransitHandler]]))


(def transit-read-handlers
  {"DbError" read-DbError
   "DbVal" read-DbVal
   "Entity" read-Entity
   "->entity" read-ThinEntity
   "EReq" read-EntityRequest
   "QReq" read-QueryRequest
   "r" read-URI})

(def transit-write-handlers
  {URI (URITransitHandler.)
   DbVal (DbValTransitHandler.)
   DbError (DbErrorTransitHandler.)
   Entity (EntityTransitHandler.)
   ThinEntity (ThinEntityTransitHandler.)
   QueryRequest (QueryRequestTransitHandler.)
   EntityRequest (EntityRequestTransitHandler.)})

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
