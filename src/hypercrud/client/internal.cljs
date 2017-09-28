(ns hypercrud.client.internal
  (:require [cognitect.transit :as t]
            [hypercrud.types.DbId :refer [DbId DbIdTransitReader DbIdTransitHandler]]
            [hypercrud.types.DbVal :refer [DbVal DbValTransitReader DbValTransitHandler]]
            [hypercrud.types.DbError :refer [DbError DbErrorTransitReader DbErrorTransitHandler]]
            [hypercrud.types.QueryRequest :refer [QueryRequest read-QueryRequest QueryRequestTransitHandler]]
            [hypercrud.types.EntityRequest :refer [EntityRequest read-EntityRequest EntityRequestTransitHandler]]
            [hypercrud.types.URI :refer [URI URITransitReader URITransitHandler]]))


(def transit-read-handlers
  {"r" URITransitReader
   "DbId" DbIdTransitReader
   "DbVal" DbValTransitReader
   "DbError" DbErrorTransitReader
   "QReq" read-QueryRequest
   "EReq" read-EntityRequest})

(def transit-write-handlers
  {URI (URITransitHandler.)
   DbId (DbIdTransitHandler.)
   DbVal (DbValTransitHandler.)
   DbError (DbErrorTransitHandler.)
   QueryRequest (QueryRequestTransitHandler.)
   EntityRequest (EntityRequestTransitHandler.)})


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
