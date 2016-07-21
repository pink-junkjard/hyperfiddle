(ns hypercrud.client.http
  (:require [cljs.core.match :refer-macros [match]]
            [goog.Uri]
            [hypercrud.client.core :as hc]
            [hypercrud.client.graph :as graph]
            [hypercrud.client.internal :as internal]
            [hypercrud.client.tx :as tx]
            [hypercrud.client.util :as util]
            [kvlt.core :as kvlt]
            [kvlt.middleware.params]
            [promesa.core :as p]))


(def content-type-transit "application/transit+json;charset=UTF-8")


(defmethod kvlt.middleware.params/coerce-form-params (keyword content-type-transit) [{:keys [form-params]}]
  (internal/transit-encode form-params))


(defmethod kvlt.middleware/from-content-type (keyword content-type-transit) [resp]
  (let [decoded-val (internal/transit-decode (:body resp))]
    (assoc resp :body decoded-val)))


(defn resolve-root-relative-uri [^goog.Uri entry-uri ^goog.Uri relative-uri]
  (-> (.clone entry-uri)
      (.resolve relative-uri)))


; graph is always assumed to be touched
(deftype Client [^:mutable user-token entry-uri schema ^:mutable graph]
  hc/Client
  (authenticate! [this username password]
    (set! user-token "dustin"))


  (whoami [this] user-token)


  (graph [this]
    (assert (not= nil graph) "invariant - runtime must call enter! first")
    graph)


  (enter! [this named-queries t]
    ;; compare our pre-loaded state with the graph dependencies
    (let [graph-we-want (graph/Graph. schema named-queries t [] nil)]
      (if (= graph graph-we-want)
        (p/resolved graph)
        (-> (kvlt/request!
              {:url (-> (resolve-root-relative-uri entry-uri (goog.Uri. "/api/enter?tx=" t))
                        (.setParameterValue "user-token" user-token))
               :content-type content-type-transit
               :accept content-type-transit
               :method :post
               :form (into [] (vals named-queries))
               :as :auto})
            (p/then #(do
                      (graph/touch! graph-we-want (-> % :body :hypercrud))
                      (set! graph graph-we-want)
                      graph))))))


  (transact! [this tx]
    (kvlt/request!
      {:url (-> (resolve-root-relative-uri entry-uri (goog.Uri. "/api/transact"))
                (.setParameterValue "user-token" user-token))
       :content-type content-type-transit
       :accept content-type-transit
       :method :post
       :form tx
       :as :auto})))
