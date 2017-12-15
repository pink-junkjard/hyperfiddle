(ns hypercrud.http.kvlt-config
  (:require
    #?(:cljs [cljs.pprint :as pprint])
    #?(:cljs [hypercrud.client.transit :as transit])
    [hypercrud.compile.reader :as reader]
    [kvlt.middleware]
    [kvlt.middleware.params]))


(defmethod kvlt.middleware.params/coerce-form-params
  (keyword "application/transit+json")
  [{:keys [form-params]}]
  #?(:clj  (assert false "todo")
     :cljs (transit/encode form-params)))

(defmethod kvlt.middleware/from-content-type
  (keyword "application/transit+json; charset=utf-8")
  [resp]
  #?(:clj  (assert false "todo")
     :cljs (update resp :body transit/decode)))

(defmethod kvlt.middleware/from-content-type
  (keyword "application/transit+json")
  [resp]
  #?(:clj  (assert false "todo")
     :cljs (update resp :body transit/decode)))

(defmethod kvlt.middleware.params/coerce-form-params :application/edn [{:keys [form-params]}]
  #?(:clj  (assert false "todo")
     :cljs (binding [pprint/*print-miser-width* nil
                     pprint/*print-right-margin* 200]
             (with-out-str (pprint/pprint form-params)))))

(defmethod kvlt.middleware/from-content-type :application/edn [resp]
  (let [decoded-val (reader/read-edn-string (:body resp))]      ; todo this can throw
    (assoc resp :body decoded-val)))
