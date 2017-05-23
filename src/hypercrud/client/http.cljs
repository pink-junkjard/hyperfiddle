(ns hypercrud.client.http
  (:require [cljs.pprint :as pprint]
            [cljs.reader :as reader]
            [clojure.set :as set]
            [goog.Uri]
            [hypercrud.client.core :as hc]
            [hypercrud.client.internal :as internal]
            [hypercrud.client.peer :as peer]
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


(deftype HttpConnection [entry-uri ^:mutable peer]
  hc/Connection
  (hydrate!* [this requests staged-tx]
    (-> (kvlt/request! {:url (resolve-relative-uri entry-uri (goog.Uri. "hydrate"))
                        :content-type content-type-transit ; helps debugging to view as edn
                        :accept content-type-transit ; needs to be fast so transit
                        :method :post
                        :form {:staged-tx staged-tx :request (into #{} requests)}
                        :as :auto})
        (p/then (fn [http-response]
                  (let [{:keys [t pulled-trees-map]} (-> http-response :body :hypercrud)]
                    (peer/->Peer (into #{} requests) pulled-trees-map))))))

  (hydrate! [this requests staged-tx force?]
    (if (and (not force?) (hc/hydrated? this requests))
      (p/resolved peer)
      (-> (hc/hydrate!* this requests staged-tx)
          (p/then (fn [peer']
                    (set! peer peer')
                    peer)))))

  ; for clone link - is this bad? yeah its bad since it can never be batched.
  (hydrate-one! [this request staged-tx]
    (-> (hc/hydrate!* this #{request} staged-tx)
        (p/then (fn [peer] (hypercrud.client.core/hydrate peer request)))))


  (hydrated? [this requests]
    ; compare our pre-loaded state with the peer dependencies
    (set/subset? (set requests) (some-> peer .-requests)))


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
