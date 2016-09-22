(ns hypercrud.client.http
  (:require [goog.Uri]
            [hypercrud.client.core :as hc]
            [hypercrud.client.graph :as graph]
            [hypercrud.client.internal :as internal]
            [hypercrud.form.util :as form-util]
            [kvlt.core :as kvlt]
            [kvlt.middleware.params]
            [promesa.core :as p]))


(def content-type-transit "application/transit+json;charset=UTF-8")


(defmethod kvlt.middleware.params/coerce-form-params (keyword content-type-transit) [{:keys [form-params]}]
  (internal/transit-encode form-params))


(defmethod kvlt.middleware/from-content-type (keyword content-type-transit) [resp]
  (let [decoded-val (internal/transit-decode (:body resp))]
    (assoc resp :body decoded-val)))


(defn resolve-relative-uri [^goog.Uri entry-uri ^goog.Uri relative-uri]
  (-> (.clone entry-uri)
      (.resolve relative-uri)))


; graph is always assumed to be touched
(deftype Client [entry-uri schema ^:mutable graph temp-id-atom]
  hc/Client
  (graph [this]
    (assert (not= nil graph) "invariant - runtime must call hydrate! first")
    graph)


  (hydrate! [this named-queries t]
    ;; compare our pre-loaded state with the graph dependencies
    (let [graph-we-want (graph/->Graph schema named-queries t [] nil)]
      (if (= graph graph-we-want)
        (p/resolved graph)
        (do
          (doseq [[q p _] (vals named-queries)]
            (assert (= (count (form-util/parse-holes q))
                       (count p))
                    (str "Missing parameters for " q)))
          (-> (kvlt/request!
                {:url (let [url (resolve-relative-uri entry-uri (goog.Uri. "hydrate"))]
                        (if t (.setParameterValue url "t" t))
                        url)
                 :content-type content-type-transit
                 :accept content-type-transit
                 :method :post
                 :form (into [] (vals named-queries))
                 :as :auto})
              (p/then (fn [resp]
                        (let [{:keys [t pulled-trees-map]} (-> resp :body :hypercrud)]
                          (graph/set-state! graph-we-want pulled-trees-map t)
                          (set! graph graph-we-want)
                          graph))))))))


  (temp-id! [this]
    (swap! temp-id-atom dec))


  (transact! [this tx]
    (-> (kvlt/request!
          {:url (resolve-relative-uri entry-uri (goog.Uri. "transact"))
           :content-type content-type-transit
           :accept content-type-transit
           :method :post
           :form tx
           :as :auto})
        (p/then (fn [resp]
                  (if (:success resp)
                    (p/resolved (-> resp :body :hypercrud))
                    (p/rejected resp)))))))
