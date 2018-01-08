(ns hypercrud.http.core
  (:require
    [hypercrud.http.kvlt-config]
    [hypercrud.util.performance :as perf]
    [kvlt.core :as kvlt]
    [taoensso.timbre :as timbre]))


(defn http-request! [req]
  (let [req-hash (delay (hash req))]
    (perf/time-promise
      (do
        (timbre/debug "Issuing request" (str "[" @req-hash "]") (:url req))
        ; todo inject a request-id to track on backend
        (kvlt/request! req))
      (fn [_ get-total-time]
        (timbre/debug "Request failed" (str "[" @req-hash "]") "total time:" (get-total-time)))
      (fn [_ get-total-time]
        (timbre/debug "Request succeeded" (str "[" @req-hash "]") "total time:" (get-total-time))))))
