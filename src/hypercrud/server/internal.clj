(ns hypercrud.server.internal
  (:require [cognitect.transit :as t]
            [hypercrud.transit :as hc-t]
            [hypercrud.types.Entity :refer [read-Entity]]
            [hypercrud.types.URI])
  (:import (hypercrud.types.Entity Entity)
           (java.net URI)))


(def transit-read-handlers
  (merge hc-t/read-handlers
         {"Entity" (t/read-handler read-Entity)
          "r" (t/read-handler #(URI. %))}))

(def transit-write-handlers
  (merge hc-t/write-handlers
         {Entity
          (t/write-handler (constantly "Entity") (fn [v] [(.-dbval v) (.-coll v)]))}))
