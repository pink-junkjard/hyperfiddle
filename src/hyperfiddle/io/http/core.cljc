(ns hyperfiddle.io.http.core
  (:require
    [contrib.performance :as perf]
    [hypercrud.types.Err :as Err]
    #?(:cljs [hyperfiddle.io.http.kvlt-config])
    #?(:cljs [kvlt.core :as kvlt])
    [promesa.core :as p]
    [taoensso.timbre :as timbre]))


(defn http-request! [req]
  (let [req-hash (delay (hash req))]
    (perf/time-promise
      (do
        (timbre/debug "Issuing request" (str "[" @req-hash "]") (:url req))
        ; todo inject a request-id to track on backend
        #?(:clj  (assert false "kvlt broken on jvm")
           :cljs (-> (kvlt/request! (assoc req :kvlt.platform/credentials? true))
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

                                    (and (= 504 (:status data)) (not #?(:clj (.getMessage e) :cljs (ex-message e))))
                                    (throw (ex-info "Service timed out"
                                                    (assoc data :hyperfiddle.io/http-status-code 504)
                                                    #?(:clj (.getCause e) :cljs (ex-cause e))))

                                    (= (:status data) 0) (throw (ex-info (ex-message e) data (ex-cause e)))

                                    (:status data) (throw (ex-info (ex-message e)
                                                                   (assoc data :hyperfiddle.io/http-status-code (:status data))
                                                                   (ex-cause e)))

                                    :else (throw e))))))))
      (fn [_ get-total-time]
        (timbre/debug "Request failed" (str "[" @req-hash "]") "total time:" (get-total-time)))
      (fn [_ get-total-time]
        (timbre/debug "Request succeeded" (str "[" @req-hash "]") "total time:" (get-total-time))))))
