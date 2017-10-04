(ns hypercrud.server.internal
  (:require [hypercrud.types.DbId]
            [hypercrud.types.DbVal]
            [hypercrud.types.DbError]
            [hypercrud.types.EntityRequest]
            [hypercrud.types.QueryRequest]
            [hypercrud.types.URI])
  (:import (hypercrud.types.DbId DbId DbIdTransitHandler DbIdTransitReader)
           (hypercrud.types.DbVal DbVal DbValTransitHandler DbValTransitReader)
           (hypercrud.types.DbError DbError DbErrorTransitHandler DbErrorTransitReader)
           (hypercrud.types.EntityRequest EntityRequest EntityRequestTransitHandler EntityRequestTransitReader)
           (hypercrud.types.QueryRequest QueryRequest QueryRequestTransitHandler QueryRequestTransitReader)
           (com.cognitect.transit WriteHandler ReadHandler)
           (java.net URI)))


(def transit-read-handlers {"DbId" (DbIdTransitReader.)
                            "DbVal" (DbValTransitReader.)
                            "DbError" (DbErrorTransitReader.)
                            "QReq" (QueryRequestTransitReader.)
                            "EReq" (EntityRequestTransitReader.)
                            "r" (reify ReadHandler (fromRep [_ v] (URI. v)))})

(def transit-write-handlers {DbId (DbIdTransitHandler.)
                             DbVal (DbValTransitHandler.)
                             DbError (DbErrorTransitHandler.)
                             QueryRequest (QueryRequestTransitHandler.)
                             EntityRequest (EntityRequestTransitHandler.)})
