(ns hypercrud.server.internal
  (:require [cognitect.transit :as t]
            [hypercrud.transit :as hc-t]
            [hypercrud.types.URI])
  (:import (java.net URI)))


(def transit-read-handlers
  (merge hc-t/read-handlers
         {"r" (t/read-handler #(URI. %))}))

(def transit-write-handlers hc-t/write-handlers)
