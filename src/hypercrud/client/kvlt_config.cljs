(ns hypercrud.client.kvlt-config
  (:require [cljs.reader :as reader]
            [cljs.pprint :as pprint]
            [hypercrud.client.transit :as transit]
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
  (binding [pprint/*print-miser-width* nil
            pprint/*print-right-margin* 200]
    (with-out-str (pprint/pprint form-params))))

(defmethod kvlt.middleware/from-content-type :application/edn [resp]
  (let [decoded-val (reader/read-string (:body resp))]
    (assoc resp :body decoded-val)))