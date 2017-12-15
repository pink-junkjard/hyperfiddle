(ns hypercrud.transit
  (:require [cognitect.transit :as t]
            [hypercrud.types.DbVal :refer [->DbVal #?(:cljs DbVal)]]
            [hypercrud.types.Entity :refer [->Entity #?(:cljs Entity)]]
            [hypercrud.types.EntityRequest :refer [->EntityRequest #?(:cljs EntityRequest)]]
            [hypercrud.types.Err :refer [->Err #?(:cljs Err)]]
            [hypercrud.types.QueryRequest :refer [->QueryRequest #?(:cljs QueryRequest)]]
            [hypercrud.types.ThinEntity :refer [->ThinEntity #?(:cljs ThinEntity)]]
            [hypercrud.types.URI :refer [->URI #?(:cljs URI)]])
  #?(:clj
     (:import (hypercrud.types.DbVal DbVal)
              (hypercrud.types.Entity Entity)
              (hypercrud.types.EntityRequest EntityRequest)
              (hypercrud.types.Err Err)
              (hypercrud.types.QueryRequest QueryRequest)
              (hypercrud.types.ThinEntity ThinEntity))))


(def read-handlers
  {"DbVal" (t/read-handler #(apply ->DbVal %))
   "Entity" (t/read-handler #(apply ->Entity %))
   "EReq" (t/read-handler #(apply ->EntityRequest %))
   "err" (t/read-handler ->Err)
   "QReq" (t/read-handler #(apply ->QueryRequest %))
   "entity" (t/read-handler #(apply ->ThinEntity %))
   "r" (t/read-handler ->URI)})

(def write-handlers
  {DbVal
   (t/write-handler (constantly "DbVal") (fn [v] [(:uri v) (:branch v)]))

   Entity
   (t/write-handler (constantly "Entity") (fn [v] [(.-dbval v) (.-coll v)]))

   EntityRequest
   (t/write-handler (constantly "EReq") (fn [v] [(:e v) (:a v) (:db v) (:pull-exp v)]))

   Err
   (t/write-handler (constantly "err") #(:msg %))

   QueryRequest
   (t/write-handler (constantly "QReq") (fn [v] [(:query v) (:params v) (:pull-exps v)]))

   ThinEntity
   (t/write-handler (constantly "entity") (fn [v] [(.-dbname v) (.-id v)]))

   #?@(:cljs
       [URI
        (t/write-handler (constantly "r") (fn [v] (.-uri-str v)))])})

