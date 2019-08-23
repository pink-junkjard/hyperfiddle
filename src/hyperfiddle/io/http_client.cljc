(ns hyperfiddle.io.http-client
  (:require
    [contrib.base-64-url-safe :as base-64-url-safe]
    [contrib.ednish :as ednish]
    [contrib.reader :as reader]
    [contrib.performance :as perf]
    [cuerdas.core :as str]
    #?(:cljs [kvlt.core :as kvlt])
    #?(:cljs [kvlt.middleware])
    #?(:cljs [kvlt.middleware.params])
    [hypercrud.transit :as transit]
    [hypercrud.types.Err :as Err]
    [hyperfiddle.domain :as domain]
    [promesa.core :as p]
    [taoensso.timbre :as timbre]))


#?(:cljs
   (defmethod kvlt.middleware.params/coerce-form-params
     (keyword "application/transit+json")
     [{:keys [form-params]}]
     (transit/encode form-params)))

#?(:cljs
   (defmethod kvlt.middleware/from-content-type
     (keyword "application/transit+json; charset=utf-8")
     [resp]
     (update resp :body transit/decode)))

#?(:cljs
   (defmethod kvlt.middleware/from-content-type
     (keyword "application/transit+json")
     [resp]
     (update resp :body transit/decode)))

#?(:cljs
   (defmethod kvlt.middleware.params/coerce-form-params :application/edn [{:keys [form-params]}]
     ; hyperfiddle/hyperfiddle.net#38
     (pr-str form-params)))

#?(:cljs
   (defmethod kvlt.middleware/from-content-type :application/edn [resp]
     (let [decoded-val (reader/read-edn-string! (:body resp))] ; todo this can throw
       (assoc resp :body decoded-val))))

(def ^:dynamic *force-refresh*)

(defn http-request! [req & [jwt]]
  (let [req (cond-> req
              jwt (assoc :auth {:bearer jwt}))
        req-hash (delay (hash req))]
    (perf/time-promise
      (do
        (timbre/debugf "Request[%s] issued to %s" @req-hash (:url req))
        ; todo inject a request-id to track on backend
        #?(:clj  (assert false "kvlt broken on jvm")
           :cljs (-> (kvlt/request! req)
                     (p/catch (fn [e]
                                (let [data (ex-data e)
                                      response-body (:body data)]
                                  (cond
                                    (Err/Err? response-body)
                                    (throw (ex-info (:msg response-body)
                                                    (assoc data :hyperfiddle.io/http-status-code (:status data))
                                                    (ex-cause e)))

                                    (and (= 502 (:status data)) (not (ex-message e)))
                                    (throw (ex-info "Service Unavailable"
                                                    (assoc data :hyperfiddle.io/http-status-code 502)
                                                    (ex-cause e)))

                                    (and (= 504 (:status data)) (not (ex-message e)))
                                    (throw (ex-info "Service timed out"
                                                    (assoc data :hyperfiddle.io/http-status-code 504)
                                                    (ex-cause e)))

                                    (= (:status data) 0) (throw (ex-info (ex-message e) data (ex-cause e)))

                                    (and (= (:status data) 404)
                                         (some? *force-refresh*)
                                         (= "Please refresh your browser" response-body))
                                    (*force-refresh* (ex-info response-body
                                                              (assoc data :hyperfiddle.io/http-status-code (:status data))
                                                              (ex-cause e)))

                                    (:status data) (throw (ex-info (ex-message e)
                                                                   (assoc data :hyperfiddle.io/http-status-code (:status data))
                                                                   (ex-cause e)))

                                    :else (throw e))))))))
      (fn [_ total-time]
        (timbre/debugf "Request[%s] failed, total time: %sms" @req-hash total-time))
      (fn [_ total-time]
        (timbre/debugf "Request[%s] succeeded, total time: %sms" @req-hash total-time)))))

(defn global-basis! [domain ?service-uri & [jwt]]
  (-> {:url (str ?service-uri (domain/api-path-for domain :global-basis))
       :accept :application/transit+json :as :auto
       :method :get}
      (http-request! jwt)
      (p/then :body)))

(defn hydrate-requests! [domain ?service-uri local-basis partitions requests & [jwt]]
  (let [req {:url (str ?service-uri (domain/api-path-for domain :hydrate-requests :local-basis (ednish/encode-uri local-basis))) ; serialize kvseq
             :accept :application/transit+json :as :auto
             :method :post                                  ; hydrate-requests always has a POST body, though it has a basis and is cachable
             :form {:partitions partitions :request requests}
             :content-type :application/transit+json}]
    (timbre/debugf "hydrate-requests! request count= %s basis= %s form= %s" (count requests) (pr-str local-basis) (str/prune (pr-str (:form req)) 100))
    (-> (http-request! req)
        (p/then (fn [{:keys [body]}]
                  (assert (= (count requests) (count (:pulled-trees body))) "Server contract violation; mismatched counts")
                  body)))))

(defn hydrate-route! [domain ?service-uri local-basis route pid partitions & [jwt]]
  {:pre [domain local-basis route]}
  (-> (merge {:url (str ?service-uri
                        (domain/api-path-for domain :hydrate-route
                                             :local-basis (ednish/encode-uri local-basis)
                                             ; todo this needs work
                                             #_#_:encoded-route (subs (foundation/route-encode rt route) 1) ; includes "/"
                                             :encoded-route (base-64-url-safe/encode (pr-str route))
                                             :partition-id (base-64-url-safe/encode (pr-str pid))))
              :accept :application/transit+json :as :auto}
             (if false #_(empty? stage)                     ; todo
               {:method :get}                               ; Try to hit CDN
               {:method :post
                :form partitions
                :content-type :application/transit+json}))
      (http-request! jwt)
      (p/then :body)))

(defn local-basis! [domain ?service-uri global-basis route & [jwt]]
  (-> {:url (str ?service-uri (domain/api-path-for domain :local-basis
                                                   :global-basis (ednish/encode-uri global-basis)
                                                   ; todo this needs work
                                                   #_#_:encoded-route (subs (foundation/route-encode rt route) 1) ; includes "/"
                                                   :encoded-route (base-64-url-safe/encode (pr-str route))))
       :accept :application/transit+json :as :auto
       :method :get}
      (http-request! jwt)
      (p/then :body)))

(defn sync! [domain ?service-uri dbnames & [jwt]]
  (-> {:url (str ?service-uri (domain/api-path-for domain :sync))
       :accept :application/transit+json :as :auto
       :method :post :form dbnames
       :content-type :application/transit+json}
      (http-request! jwt)
      (p/then :body)))

(defn transact! [domain ?service-uri tx-groups & [jwt]]
  (-> {:url (str ?service-uri (domain/api-path-for domain :transact))
       :accept :application/transit+json :as :auto
       :method :post :form tx-groups
       :content-type :application/transit+json}
      (http-request! jwt)
      (p/then (fn [resp]
                (if (= 200 (:status resp))
                  ; clear master stage
                  ; but that has to be transactional with a redirect???
                  (p/resolved (:body resp))
                  (p/rejected resp))))))
