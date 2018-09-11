(ns hypercrud.transit
  (:require [cognitect.transit :as t]
            [hypercrud.types.DbVal :refer [->DbVal #?(:cljs DbVal)]]
            [hypercrud.types.EntityRequest :refer [->EntityRequest #?(:cljs EntityRequest)]]
            [hypercrud.types.Err :refer [->Err #?(:cljs Err)]]
            [hypercrud.types.QueryRequest :refer [->QueryRequest #?(:cljs QueryRequest)]]
            [hypercrud.types.ThinEntity :refer [->ThinEntity #?(:cljs ThinEntity)]]
            [contrib.uri :refer [->URI #?(:cljs URI)]]
            [hyperfiddle.runtime :refer [map->HostEnvironment #?(:cljs HostEnvironment)]])
  #?(:clj
     (:import (hypercrud.types.DbVal DbVal)
              (hypercrud.types.EntityRequest EntityRequest)
              (hypercrud.types.Err Err)
              (hypercrud.types.QueryRequest QueryRequest)
              (hypercrud.types.ThinEntity ThinEntity)
              (hyperfiddle.runtime HostEnvironment)
              (java.io ByteArrayInputStream ByteArrayOutputStream))))


(def read-handlers
  {"DbVal" (t/read-handler #(apply ->DbVal %))
   "EReq" (t/read-handler #(apply ->EntityRequest %))
   "err" (t/read-handler ->Err)
   "QReq" (t/read-handler #(apply ->QueryRequest %))
   "entity" (t/read-handler #(apply ->ThinEntity %))
   "r" (t/read-handler ->URI)
   "HostEnvironment" (t/read-handler map->HostEnvironment)})

(def write-handlers
  {DbVal
   (t/write-handler (constantly "DbVal") (fn [v] [(:uri v) (:branch v)]))

   EntityRequest
   (t/write-handler (constantly "EReq") (fn [v] [(:e v) (:db v) (:pull-exp v)]))

   Err
   (t/write-handler (constantly "err") #(:msg %))

   QueryRequest
   (t/write-handler (constantly "QReq") (fn [v] [(:query v) (:params v)]))

   ThinEntity
   (t/write-handler (constantly "entity") (fn [v] [(.-dbname v) (.-id v)]))

   HostEnvironment
   (t/write-handler (constantly "HostEnvironment") #(into {} %))

   #?@(:cljs
       [URI
        (t/write-handler (constantly "r") (fn [v] (.-uri-str v)))])})

(def ^:dynamic *string-encoding* "UTF-8")

(defn decode
  "Transit decode an object from `s`."
  [s & {:keys [type opts]
        :or {type :json opts {:handlers read-handlers}}}]
  #?(:clj  (let [in (ByteArrayInputStream. (.getBytes s *string-encoding*))
                 rdr (t/reader in type opts)]
             (t/read rdr))
     :cljs (let [rdr (t/reader type opts)]
             (t/read rdr s))))

(defn encode
  "Transit encode `x` into a String."
  [x & {:keys [type opts]
        :or {type :json opts {:handlers write-handlers}}}]
  #?(:clj  (let [out (ByteArrayOutputStream.)
                 writer (t/writer out type opts)]
             (t/write writer x)
             (.toString out))
     :cljs (let [wrtr (t/writer type opts)]
             (t/write wrtr x))))
