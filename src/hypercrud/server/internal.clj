(ns hypercrud.server.internal
  (:require [hypercrud.types.DbId]
            [hypercrud.types.DbVal]
            [hypercrud.types.DbError]
            [hypercrud.types.EntityRequest]
            [hypercrud.types.QueryRequest])
  (:import (hypercrud.types.DbId DbId DbIdTransitHandler DbIdTransitReader)
           (hypercrud.types.DbVal DbVal DbValTransitHandler DbValTransitReader)
           (hypercrud.types.DbError DbError DbErrorTransitHandler DbErrorTransitReader)
           (hypercrud.types.EntityRequest EntityRequest EntityRequestTransitHandler EntityRequestTransitReader)
           (hypercrud.types.QueryRequest QueryRequest QueryRequestTransitHandler QueryRequestTransitReader)))


(def transit-read-handlers {"DbId" (DbIdTransitReader.)
                            "DbVal" (DbValTransitReader.)
                            "DbError" (DbErrorTransitReader.)
                            "QReq" (QueryRequestTransitReader.)
                            "EReq" (EntityRequestTransitReader.)})

(def transit-write-handlers {DbId (DbIdTransitHandler.)
                             DbVal (DbValTransitHandler.)
                             DbError (DbErrorTransitHandler.)
                             QueryRequest (QueryRequestTransitHandler.)
                             EntityRequest (EntityRequestTransitHandler.)})
