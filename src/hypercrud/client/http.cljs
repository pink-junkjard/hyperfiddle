(ns hypercrud.client.http
  (:require [cljs.pprint :as pprint]
            [cljs.reader :as reader]
            [clojure.set :as set]
            [goog.Uri]
            [hypercrud.client.core :as hc]
            [hypercrud.client.graph :as graph]
            [hypercrud.client.internal :as internal]
            [hypercrud.client.schema :as schema-util]
            [hypercrud.types :as types :refer [->DbId ->DbVal]]
            [kvlt.core :as kvlt]
            [kvlt.middleware.params]
            [promesa.core :as p]))


(def content-type-transit "application/transit+json;charset=UTF-8")
(def content-type-edn "application/edn;charset=UTF-8")


(defmethod kvlt.middleware.params/coerce-form-params (keyword content-type-transit) [{:keys [form-params]}]
  (internal/transit-encode form-params))


(defmethod kvlt.middleware/from-content-type (keyword content-type-transit) [resp]
  (let [decoded-val (internal/transit-decode (:body resp))]
    (assoc resp :body decoded-val)))


(defmethod kvlt.middleware.params/coerce-form-params (keyword content-type-edn) [{:keys [form-params]}]
  (binding [pprint/*print-miser-width* nil
            pprint/*print-right-margin* 200]
    (with-out-str (pprint/pprint form-params))))


(defmethod kvlt.middleware/from-content-type (keyword content-type-edn) [resp]
  (let [decoded-val (reader/read-string (:body resp))]
    (assoc resp :body decoded-val)))


(defn resolve-relative-uri [^goog.Uri entry-uri ^goog.Uri relative-uri]
  (-> (.clone entry-uri)
      (.resolve relative-uri)))


; graph is always assumed to be touched
(deftype Client [entry-uri ^:mutable super-graph]
  hc/Client
  (hydrate! [this request force? staged-tx editor-dbval editor-schema]
    (if (and (not force?) (hc/hydrated? this request))
      (p/resolved super-graph)
      (-> (kvlt/request!
            {:url (resolve-relative-uri entry-uri (goog.Uri. "hydrate"))
             :content-type content-type-transit             ; helps debugging to view as edn
             :accept content-type-transit                   ; needs to be fast so transit
             :method :post
             :form {:staged-tx staged-tx
                    :request (->> request
                                  (mapv (fn [req-or-dbval]
                                          (if (instance? types/DbVal req-or-dbval)
                                            (schema-util/schema-request editor-dbval req-or-dbval)
                                            req-or-dbval)))
                                  (into #{}))}
             :as :auto})
          (p/then (fn [resp]
                    (let [new-graph (graph/->SuperGraph (into #{} request) {} nil nil)
                          {:keys [t pulled-trees-map tempids]} (-> resp :body :hypercrud)]
                      (graph/set-state! new-graph editor-dbval editor-schema pulled-trees-map tempids)
                      (set! super-graph new-graph)
                      super-graph))))))


  (hydrated? [this request]
    ; compare our pre-loaded state with the graph dependencies
    (set/subset? (set request) (some-> super-graph .-request)))


  (transact! [this tx]
    (-> (kvlt/request!
          {:url (resolve-relative-uri entry-uri (goog.Uri. "transact"))
           :content-type content-type-edn
           :accept content-type-edn
           :method :post
           :form tx
           :as :auto})
        (p/then (fn [resp]
                  (if (:success resp)
                    (p/resolved (-> resp :body :hypercrud))
                    (p/rejected resp)))))))
