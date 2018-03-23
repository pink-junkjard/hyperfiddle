(ns hyperfiddle.io.http.core
  (:require
    #?(:cljs [hyperfiddle.io.http.kvlt-config])
    [hypercrud.types.Err :as Err]
    [hypercrud.util.performance :as perf]
    #?(:cljs [kvlt.core :as kvlt])
    [taoensso.timbre :as timbre]
    [promesa.core :as p]))


(defn http-request! [req]
  (let [req-hash (delay (hash req))]
    (perf/time-promise
      (do
        (timbre/debug "Issuing request" (str "[" @req-hash "]") (:url req))
        ; todo inject a request-id to track on backend
        #?(:clj  (assert false "kvlt broken on jvm")
           :cljs (-> (kvlt/request! req)
                     (p/catch (fn [e]
                                (let [data (ex-data e)
                                      response-body (:body data)]
                                  (cond
                                    (Err/Err? response-body)
                                    (throw (ex-info (:msg response-body)
                                                    (assoc data :hyperfiddle.io/http-status-code (:status data))
                                                    #?(:clj (.getCause e) :cljs (ex-cause e))))

                                    (and (= 502 (:status data)) (not #?(:clj (.getMessage e) :cljs (ex-message e))))
                                    (throw (ex-info "Service Unavailable"
                                                    (assoc data :hyperfiddle.io/http-status-code 502)
                                                    #?(:clj (.getCause e) :cljs (ex-cause e))))

                                    (:status data) (throw (ex-info (ex-message e)
                                                                   (assoc data :hyperfiddle.io/http-status-code (:status data))
                                                                   (ex-cause e)))

                                    :else (throw e))))))))
      (fn [_ get-total-time]
        (timbre/debug "Request failed" (str "[" @req-hash "]") "total time:" (get-total-time)))
      (fn [_ get-total-time]
        (timbre/debug "Request succeeded" (str "[" @req-hash "]") "total time:" (get-total-time))))))
