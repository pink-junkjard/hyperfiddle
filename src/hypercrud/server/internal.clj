(ns hypercrud.server.internal
  (:require [hypercrud.transit :as hc-t]
            [hypercrud.types.DbVal]
            [hypercrud.types.Entity]
            [hypercrud.types.EntityRequest]
            [hypercrud.types.QueryRequest]
            [hypercrud.types.URI])
  (:import (hypercrud.types.DbVal DbVal DbValTransitHandler DbValTransitReader)
           (hypercrud.types.Entity Entity EntityTransitHandler EntityTransitReader)
           (hypercrud.types.EntityRequest EntityRequest EntityRequestTransitHandler EntityRequestTransitReader)
           (hypercrud.types.QueryRequest QueryRequest QueryRequestTransitHandler QueryRequestTransitReader)
           (com.cognitect.transit WriteHandler ReadHandler)
           (java.net URI)))


(def transit-read-handlers
  (merge hc-t/read-handlers
         {"DbVal" (DbValTransitReader.)
          "Entity" (EntityTransitReader.)
          "QReq" (QueryRequestTransitReader.)
          "EReq" (EntityRequestTransitReader.)
          "r" (reify ReadHandler (fromRep [_ v] (URI. v)))}))

(def transit-write-handlers
  (merge hc-t/write-handlers
         {DbVal (DbValTransitHandler.)
          Entity (EntityTransitHandler.)
          QueryRequest (QueryRequestTransitHandler.)
          EntityRequest (EntityRequestTransitHandler.)}))
