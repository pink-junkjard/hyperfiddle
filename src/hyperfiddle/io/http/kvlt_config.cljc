(ns hyperfiddle.io.http.kvlt-config
  (:require [contrib.reader :refer [read-edn-string!]]
            [hypercrud.transit :as transit]
            [kvlt.middleware]
            [kvlt.middleware.params]))


(defmethod kvlt.middleware.params/coerce-form-params
  (keyword "application/transit+json")
  [{:keys [form-params]}]
  (transit/encode form-params))

(defmethod kvlt.middleware/from-content-type
  (keyword "application/transit+json; charset=utf-8")
  [resp]
  (update resp :body transit/decode))

(defmethod kvlt.middleware/from-content-type
  (keyword "application/transit+json")
  [resp]
  (update resp :body transit/decode))

(defmethod kvlt.middleware.params/coerce-form-params :application/edn [{:keys [form-params]}]
  ; hyperfiddle/hyperfiddle.net#38
  (pr-str form-params))

(defmethod kvlt.middleware/from-content-type :application/edn [resp]
  (let [decoded-val (read-edn-string! (:body resp))]         ; todo this can throw
    (assoc resp :body decoded-val)))
