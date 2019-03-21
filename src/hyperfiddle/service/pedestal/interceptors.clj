(ns hyperfiddle.service.pedestal.interceptors
  (:require
    [clojure.core.async :refer [chan >!!]]
    [cognitect.transit :as transit]
    [contrib.uri :refer [->URI]]
    [hypercrud.transit :as hc-t]
    [hypercrud.types.Err :refer [->Err]]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.service.cookie :as cookie]
    [hyperfiddle.service.domain :as service-domain]
    [hyperfiddle.service.http :refer [handle-route]]
    [hyperfiddle.service.jwt :as jwt]
    [io.pedestal.http :as pedestal-http]
    [io.pedestal.http.content-negotiation :as content-negotiation]
    [io.pedestal.interceptor.chain :refer [enqueue terminate]]
    [io.pedestal.interceptor.helpers :as interceptor]
    [promesa.core :as p]
    [taoensso.timbre :as timbre])
  (:import
    [com.auth0.jwt.exceptions JWTVerificationException]
    [java.io OutputStreamWriter OutputStream]
    java.util.UUID
    org.apache.commons.lang3.StringEscapeUtils))


(defn e->response [e]
  ; todo there are a subset of requests that are cacheable
  {:status (or (:hyperfiddle.io/http-status-code (ex-data e)) 500)
   :headers {}                                              ; todo retry-after on 503
   :body (->Err (.getMessage e))})

(defn service-uri [env req]
  ; we are partially trusting the request for generating a service uri which is brittle
  ; todo there should be no dependency on the request once the domain is acquired
  (let [scheme (:PUBLIC_SERVICE_HTTP_SCHEME env)
        port (:PUBLIC_SERVICE_HTTP_PORT env)]
    (-> (str scheme "://" (:server-name req))
        (cond-> (or (and (= scheme :http) (not= port 80))
                    (and (= scheme :https) (not= port 443)))
                (str ":" port))
        ->URI)))

(defn platform->pedestal-req-handler [env platform-req-handler req]
  (platform-req-handler
    :domain (:domain req)
    :route-params (:route-params req)
    :request-body (:body-params req)
    :jwt (:jwt req)
    :service-uri (service-uri env req)
    :user-id (:user-id req)))

(def combine-body-params
  (interceptor/before
    ::combine-body-params
    (fn [{{:keys [json-params edn-params transit-params]} :request :as context}]
      (assoc-in context [:request :body-params] (or json-params edn-params transit-params)))))

;; This is copied from the pedestal source code, it is private
(defn- print-fn
  [prn-fn]
  (fn [output-stream]
    (with-open [writer (OutputStreamWriter. output-stream)]
      (binding [*out* writer]
        (prn-fn))
      (.flush writer))))

(def content-transformers
  {"text/plain"
   (fn [body]
     (print-fn #(pr body)))

   "application/json"
   (fn [body]
     (print-fn #(pedestal-http/json-print body)))

   "application/edn"
   (fn [body]
     ; hyperfiddle/hyperfiddle.net#38
     (print-fn #(pr body)))

   "application/transit+json"
   (fn [body]
     (fn [^OutputStream output-stream]
       (transit/write (transit/writer output-stream :json {:handlers hc-t/write-handlers}) body)
       (.flush output-stream)))

   "application/transit+msgpack"
   (fn [body]
     (fn [^OutputStream output-stream]
       (transit/write (transit/writer output-stream :msgpack {:handlers hc-t/write-handlers}) body)
       (.flush output-stream)))

   "text/html"
   (fn [body]
     (binding [clojure.pprint/*print-right-margin* 140]
       (let [body-str (->> (with-out-str (contrib.pprint/pprint body))
                           (StringEscapeUtils/escapeHtml4)
                           (format "<html><body><pre>%s</pre></body></html>"))]
         (print-fn #(.write *out* body-str)))))})

(defn coerce-to [response content-type]
  (-> response
      (update :body (get content-transformers content-type))
      (assoc-in [:headers "Content-Type"] content-type)))

(def auto-content-type
  (interceptor/after
    ::auto-content-type
    (fn [context]
      (cond-> context
        (nil? (get-in context [:response :headers "Content-Type"]))
        (update :response coerce-to (get-in context [:request :accept :field] "text/plain"))))))

(defn promise->chan [context]
  (if-not (p/promise? (:response context))
    context
    (let [channel (chan)]
      (-> (:response context)
          (p/then (fn [response]
                    (>!! channel (assoc context :response response))))
          (p/catch (fn [err]
                     (timbre/error err)
                     (>!! channel (assoc context :response {:status 500 :body (pr-str err)})))))
      channel)))

(defn data-route [context with-request]
  (enqueue context [(content-negotiation/negotiate-content (keys content-transformers))
                    auto-content-type
                    (interceptor/after ::promise->chan promise->chan)
                    (interceptor/handler with-request)]))

(defmacro def-data-route [dispatch-val & fn-tail]
  `(defmethod handle-route ~dispatch-val [~'handler ~'env context#]
     (data-route context# (fn [~'req] ~@(rest fn-tail)))))

(defn domain [domain-for-fqdn]
  {:name ::domain
   :enter (fn [context]
            (let [channel (chan)]
              (p/branch
                (domain-for-fqdn (get-in context [:request :server-name]))
                (fn [domain]
                  (>!! channel (assoc-in context [:request :domain] domain)))
                (fn [e]
                  (timbre/error e)
                  ; todo are we sure we want to terminate completely? what about auto content type?
                  (>!! channel (assoc (terminate context) :response (e->response e)))))
              channel))})

(defn with-user-id [jwt-secret jwt-issuer]
  {:name ::with-user-id
   :enter (fn [context]
            (if-not (= "hyperfiddle" (domain/ident (get-in context [:request :domain])))
              context
              (let [#_#_{:keys [jwt-secret jwt-issuer]} (-> (get-in context [:request :domain])
                                                            domain/environment-secure :jwt)
                    verify (jwt/build-verifier jwt-secret jwt-issuer)
                    jwt-cookie (get-in context [:request :cookies "jwt" :value])
                    jwt-header (some->> (get-in context [:request :headers "authorization"])
                                        (re-find #"^Bearer (.+)$")
                                        (second))
                    with-jwt (fn [jwt]
                               (-> context
                                   (assoc-in [:request :user-id] (some-> jwt (verify) :user-id UUID/fromString))
                                   (assoc-in [:request :jwt] jwt)))]
                (cond
                  (or (nil? jwt-header) (= jwt-cookie jwt-header))
                  (try (with-jwt jwt-cookie)
                       (catch JWTVerificationException e
                         (timbre/error e)
                         (-> (terminate context)
                             (assoc :response {:status 401
                                               :cookies {"jwt" (-> (get-in context [:request :domain :ide-domain])
                                                                   (cookie/jwt-options-pedestal)
                                                                   (assoc :value jwt-cookie
                                                                          :expires "Thu, 01 Jan 1970 00:00:00 GMT"))}
                                               :body (->Err (.getMessage e))}))))

                  (nil? jwt-cookie)
                  (try (with-jwt jwt-header)
                       (catch JWTVerificationException e
                         (timbre/error e)
                         (-> (terminate context)
                             (assoc :response {:status 401 :body (->Err (.getMessage e))}))))

                  :else (-> (terminate context)
                            (assoc :response {:status 400 :body (->Err "Conflicting cookies and auth bearer")}))))))})

(defn build-router [env]
  {:name ::router
   :enter (fn [context] (service-domain/route (get-in context [:request :domain]) env context))})

(defmethod service-domain/route :default [domain env context]
  (let [path (get-in context [:request :path-info])
        request-method (get-in context [:request :request-method])
        {:keys [handler route-params]} (domain/api-match-path domain path :request-method request-method)]
    (timbre/info "router:" (pr-str handler) (pr-str request-method) (pr-str path))
    (handle-route handler env (assoc-in context [:request :route-params] route-params))))
