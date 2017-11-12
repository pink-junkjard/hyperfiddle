(ns hypercrud.server.util.http
  (:require [clojure.string :as string]
            [cognitect.transit :as transit]
            [hypercrud.transit :as hc-t]
            [io.pedestal.http :as http]
            [io.pedestal.interceptor.helpers :as interceptor])
  (:import (java.io OutputStreamWriter OutputStream)
           org.apache.commons.lang3.StringEscapeUtils))


(def combine-body-params
  (interceptor/before
    ::combine-body-params
    (fn [{{:keys [json-params edn-params transit-params]} :request :as context}]
      (assoc-in context [:request :body-params] (or json-params edn-params transit-params)))))

;; This is copied from the pedestal source code, it is private
(defn- print-fn
  [prn-fn]
  (fn [output-stream]
    (with-open [writer (OutputStreamWriter. output-stream)]
      (binding [*out* writer]
        (prn-fn))
      (.flush writer))))

;; Used for serializing the response body only; not used for parsing the request
(def content-types
  {"application/json; charset=utf-8"
   (fn [body]
     (print-fn #(http/json-print body)))

   "application/edn; charset=utf-8"
   (fn [body]
     (print-fn #(clojure.pprint/pprint body)))

   "application/transit+json; charset=utf-8"
   (fn [body]
     (fn [^OutputStream output-stream]
       (transit/write (transit/writer output-stream :json-verbose
                                      {:handlers hc-t/write-handlers}) body)
       (.flush output-stream)))

   "application/transit+msgpack; charset=utf-8"
   (fn [body]
     (fn [^OutputStream output-stream]
       (transit/write (transit/writer output-stream :msgpack) body)
       (.flush output-stream)))

   "text/html"
   (fn [body]
     (binding [clojure.pprint/*print-right-margin* 140]
       (let [body-str (as-> (with-out-str (clojure.pprint/pprint body)) $
                            (StringEscapeUtils/escapeHtml4 $)
                            (format "<html><body><pre>%s</pre></body></html>" $))]
         (print-fn #(.write *out* body-str)))))})

(def auto-content-type
  (interceptor/after
    ::auto-content-type
    (fn [{:keys [request response] :as context}]
      (let [accepts (-> (or (get-in request [:headers "accept"]) "")
                        (string/split #",")
                        (#(into #{} %)))
            accept (-> (some accepts (keys content-types))
                       #_(or "application/transit+json; charset=utf-8"))
            content-renderer (get content-types accept)]
        #_(assert content-renderer (str "invalid Accept " (get-in request [:headers "accept"])))
        (-> context
            (update-in [:response :body] (or content-renderer str))
            (update-in [:response :headers] assoc "Content-Type" accept))))))
