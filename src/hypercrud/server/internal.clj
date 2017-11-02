(ns hypercrud.server.internal
  (:require [cognitect.transit :as t]
            [hypercrud.transit :as hc-t]
            [hypercrud.types.DbVal :refer [read-DbVal]]
            [hypercrud.types.Entity :refer [read-Entity]]
            [hypercrud.types.URI])
  (:import (hypercrud.types.DbVal DbVal)
           (hypercrud.types.Entity Entity)
           (java.net URI)))


(def transit-read-handlers
  (merge hc-t/read-handlers
         {"DbVal" (t/read-handler read-DbVal)
          "Entity" (t/read-handler read-Entity)
          "r" (t/read-handler #(URI. %))}))

(def transit-write-handlers
  (merge hc-t/write-handlers
         {DbVal
          (t/write-handler (constantly "DbVal") (fn [v] [(.-uri v) (.-branch v)]))

          Entity
          (t/write-handler (constantly "Entity") (fn [v] [(.-dbval v) (.-coll v)]))}))
