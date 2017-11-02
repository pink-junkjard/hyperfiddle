(ns hypercrud.server.internal
  (:require [cognitect.transit :as t]
            [hypercrud.transit :as hc-t]
            [hypercrud.types.DbVal :refer [read-DbVal]]
            [hypercrud.types.Entity :refer [read-Entity]]
            [hypercrud.types.EntityRequest :refer [read-EntityRequest]]
            [hypercrud.types.QueryRequest :refer [read-QueryRequest]]
            [hypercrud.types.URI])
  (:import (hypercrud.types.DbVal DbVal)
           (hypercrud.types.Entity Entity)
           (hypercrud.types.EntityRequest EntityRequest)
           (hypercrud.types.QueryRequest QueryRequest)
           (java.net URI)))


(def transit-read-handlers
  (merge hc-t/read-handlers
         {"DbVal" (t/read-handler read-DbVal)
          "Entity" (t/read-handler read-Entity)
          "EReq" (t/read-handler read-EntityRequest)
          "QReq" (t/read-handler read-QueryRequest)
          "r" (t/read-handler #(URI. %))}))

(def transit-write-handlers
  (merge hc-t/write-handlers
         {DbVal
          (t/write-handler (constantly "DbVal") (fn [v] [(.-uri v) (.-branch v)]))

          Entity
          (t/write-handler (constantly "Entity") (fn [v] [(.-dbval v) (.-coll v)]))

          QueryRequest
          (t/write-handler (constantly "QReq") (fn [v] [(.-query v) (.-params v) (.-pull-exps v)]))

          EntityRequest
          (t/write-handler (constantly "EReq") (fn [v] [(.-e v) (.-a v) (.-dbval v) (.-pull-exp v)]))}))
