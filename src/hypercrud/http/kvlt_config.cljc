(ns hypercrud.http.kvlt-config
  (:require [#?(:clj clojure.pprint :cljs cljs.pprint) :as pprint]
            [hypercrud.compile.reader :as reader]
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
  (binding [pprint/*print-miser-width* nil
            pprint/*print-right-margin* 200]
    (with-out-str (pprint/pprint form-params))))

(defmethod kvlt.middleware/from-content-type :application/edn [resp]
  (let [decoded-val (reader/read-edn-string (:body resp))]  ; todo this can throw
    (assoc resp :body decoded-val)))
