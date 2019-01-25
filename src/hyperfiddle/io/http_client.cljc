(ns hyperfiddle.io.http-client
  (:require
    [bidi.bidi :as bidi]
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
    [hyperfiddle.io.routes :as routes]
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
        (timbre/debug "Issuing request" (str "[" @req-hash "]") (:url req))
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
                                                    #?(:clj (.getCause e) :cljs (ex-cause e))))

                                    (and (= 502 (:status data)) (not #?(:clj (.getMessage e) :cljs (ex-message e))))
                                    (throw (ex-info "Service Unavailable"
                                                    (assoc data :hyperfiddle.io/http-status-code 502)
                                                    #?(:clj (.getCause e) :cljs (ex-cause e))))

                                    (and (= 504 (:status data)) (not #?(:clj (.getMessage e) :cljs (ex-message e))))
                                    (throw (ex-info "Service timed out"
                                                    (assoc data :hyperfiddle.io/http-status-code 504)
                                                    #?(:clj (.getCause e) :cljs (ex-cause e))))

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
      (fn [_ get-total-time]
        (timbre/debug "Request failed" (str "[" @req-hash "]") "total time:" (get-total-time)))
      (fn [_ get-total-time]
        (timbre/debug "Request succeeded" (str "[" @req-hash "]") "total time:" (get-total-time))))))

(defn global-basis! [service-uri build & [jwt]]
  (-> {:url (str service-uri (bidi/path-for (routes/build build) :global-basis))
       :accept :application/transit+json :as :auto
       :method :get}
      (http-request! jwt)
      (p/then :body)))

(defn hydrate-requests! [service-uri build local-basis staged-branches requests & [jwt]]
  (let [req {:url (str service-uri (bidi/path-for (routes/build build) :hydrate-requests :local-basis (ednish/encode-uri local-basis))) ; serialize kvseq
             :accept :application/transit+json :as :auto
             :method :post                                  ; hydrate-requests always has a POST body, though it has a basis and is cachable
             :form {:staged-branches staged-branches :request requests}
             :content-type :application/transit+json}]
    (timbre/debugf "hydrate-requests! request count= %s basis= %s form= %s" (count requests) (pr-str local-basis) (str/prune (pr-str (:form req)) 100))
    (-> (http-request! req)
        (p/then (fn [{:keys [body]}]
                  (assert (= (count requests) (count (:pulled-trees body))) "Server contract violation; mismatched counts")
                  body)))))

(defn hydrate-route! [service-uri build local-basis route branch stage & [jwt]]
  (let [stage (->> stage
                   (remove (comp empty? second))
                   (into {}))]
    (-> (merge {:url (str service-uri (bidi/path-for (routes/build build) :hydrate-route
                                                     :local-basis (ednish/encode-uri local-basis)
                                                     ; todo this needs work
                                                     #_#_:encoded-route (subs (foundation/route-encode rt route) 1) ; includes "/"
                                                     :encoded-route (base-64-url-safe/encode (pr-str route))
                                                     :branch (ednish/encode-uri branch)))
                :accept :application/transit+json :as :auto}
               (if (empty? stage)
                 {:method :get}                             ; Try to hit CDN
                 {:method :post
                  :form stage
                  :content-type :application/transit+json}))
        (http-request! jwt)
        (p/then :body))))

(defn local-basis! [service-uri build global-basis route & [jwt]]
  (-> {:url (str service-uri (bidi/path-for (routes/build build) :local-basis
                                            :global-basis (ednish/encode-uri global-basis)
                                            ; todo this needs work
                                            #_#_:encoded-route (subs (foundation/route-encode rt route) 1) ; includes "/"
                                            :encoded-route (base-64-url-safe/encode (pr-str route))))
       :accept :application/transit+json :as :auto
       :method :get}
      (http-request! jwt)
      (p/then :body)))

(defn sync! [service-uri build dbnames & [jwt]]
  (-> {:url (str service-uri (bidi/path-for (routes/build build) :sync))
       :accept :application/transit+json :as :auto
       :method :post :form dbnames
       :content-type :application/transit+json}
      (http-request! jwt)
      (p/then :body)))

(defn transact! [service-uri build tx-groups & [jwt]]
  (-> {:url (str service-uri (bidi/path-for (routes/build build) :transact))
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
