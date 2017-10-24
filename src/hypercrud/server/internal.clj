(ns hypercrud.server.internal
  (:require [hypercrud.types.DbVal]
            [hypercrud.types.DbError]
            [hypercrud.types.Entity]
            [hypercrud.types.EntityRequest]
            [hypercrud.types.QueryRequest]
            [hypercrud.types.URI])
  (:import (hypercrud.types.DbVal DbVal DbValTransitHandler DbValTransitReader)
           (hypercrud.types.DbError DbError DbErrorTransitHandler DbErrorTransitReader)
           (hypercrud.types.Entity Entity EntityTransitHandler EntityTransitReader)
           (hypercrud.types.EntityRequest EntityRequest EntityRequestTransitHandler EntityRequestTransitReader)
           (hypercrud.types.QueryRequest QueryRequest QueryRequestTransitHandler QueryRequestTransitReader)
           (com.cognitect.transit WriteHandler ReadHandler)
           (java.net URI)))


(def transit-read-handlers {"DbVal" (DbValTransitReader.)
                            "DbError" (DbErrorTransitReader.)
                            "Entity" (EntityTransitReader.)
                            "QReq" (QueryRequestTransitReader.)
                            "EReq" (EntityRequestTransitReader.)
                            "r" (reify ReadHandler (fromRep [_ v] (URI. v)))})

(def transit-write-handlers {DbVal (DbValTransitHandler.)
                             DbError (DbErrorTransitHandler.)
                             Entity (EntityTransitHandler.)
                             QueryRequest (QueryRequestTransitHandler.)
                             EntityRequest (EntityRequestTransitHandler.)})
