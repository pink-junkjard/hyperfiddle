(ns hypercrud.client.http
  (:require [cljs.core.match :refer-macros [match]]
            [goog.Uri]
            [hypercrud.client.core :as hc]
            [hypercrud.client.graph :refer [Graph]]
            [hypercrud.client.internal :as util]
            [hypercrud.client.tx :as tx]
            [hypercrud.client.util :as wut]
            [kvlt.core :as kvlt]
            [kvlt.middleware.params]
            [promesa.core :as p]))


(def content-type-transit "application/transit+json;charset=UTF-8")


(defmethod kvlt.middleware.params/coerce-form-params (keyword content-type-transit) [{:keys [form-params]}]
  (util/transit-encode form-params))


(defmethod kvlt.middleware/from-content-type (keyword content-type-transit) [resp]
  (let [decoded-val (util/transit-decode (:body resp))]
    (assoc resp :body decoded-val)))


(defn build-hc-graph [schema pulled-trees-map]
  (let [pulled-trees (apply concat (vals pulled-trees-map)) ;mash query results together
        statements (mapcat #(tx/pulled-tree-to-statements schema %) pulled-trees)
        resultsets (wut/map-values #(map :db/id %) pulled-trees-map)]
    (Graph. schema statements resultsets [])))


(defn resolve-root-relative-uri [^goog.Uri entry-uri ^goog.Uri relative-uri]
  (-> (.clone entry-uri)
      (.resolve relative-uri)))


(deftype Client [^:mutable user-token entry-uri schema ^:mutable graph]
  hc/Client
  (authenticate! [this username password]
    (set! user-token "dustin"))


  (whoami [this] user-token)


  (graph [this]
    (assert (not= nil graph) "invariant - runtime must call enter! first")
    graph)


  (enter! [this graph-dependencies]
    (-> (kvlt/request!
          {:url (-> (resolve-root-relative-uri entry-uri (goog.Uri. "/api/enter"))
                    (.setParameterValue "user-token" user-token))
           :content-type content-type-transit
           :accept content-type-transit
           :method :post
           :form graph-dependencies
           :as :auto})
        (p/then #(set! graph (build-hc-graph schema (-> % :body :hypercrud))))))


  (transact! [this tx]
    (kvlt/request!
      {:url (-> (resolve-root-relative-uri entry-uri (goog.Uri. "/api/transact"))
                (.setParameterValue "user-token" user-token))
       :content-type content-type-transit
       :accept content-type-transit
       :method :post
       :form tx
       :as :auto})))
