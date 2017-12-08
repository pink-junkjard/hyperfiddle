(ns hypercrud.http.core
  (:require
    [hypercrud.http.kvlt-config]
    [kvlt.core :as kvlt]
    [promesa.core :as p]
    [taoensso.timbre :as timbre]))


(defn request! [req]
  (let [req-hash (delay (hash req))
        total-time #?(:clj  (assert false "todo")
                      :cljs (let [start (system-time)]
                              #(-> (- (system-time) start)
                                   (.toFixed 1)
                                   (str "ms"))))]
    (timbre/debug "Issuing request" (str "[" @req-hash "]") (:url req))
    (-> (kvlt/request! req)                                 ; todo inject a request-id to track on backend
        (p/then (fn [resp]
                  (timbre/debug "Request succeeded" (str "[" @req-hash "]") "total time:" (total-time))
                  (p/resolved resp)))
        (p/catch (fn [err]
                   (timbre/debug "Request failed" (str "[" @req-hash "]") "total time:" (total-time))
                   (p/rejected err))))))
