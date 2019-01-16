(ns hyperfiddle.service.pedestal.interceptors
  (:require
    [bidi.bidi :as bidi]
    [cats.monad.either :as either]
    [clojure.core.async :refer [chan >!!]]
    [clojure.string :as string]
    [cognitect.transit :as transit]
    [contrib.uri :refer [->URI]]
    [hypercrud.transit :as hc-t]
    [hypercrud.types.Err :refer [->Err]]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.io.core :as io]
    [hyperfiddle.io.routes :as routes]
    [hyperfiddle.service.cookie :as cookie]
    [hyperfiddle.service.http :refer [handle-route]]
    [hyperfiddle.service.jwt :as jwt]
    [io.pedestal.http :as pedestal-http]
    [io.pedestal.interceptor.chain :refer [terminate]]
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

(defn platform->pedestal-req-handler [env platform-req-handler req]
  (platform-req-handler
    :build (:BUILD env)
    :domain (:domain req)
    :service-uri (->URI (str (name (:scheme req)) "://" (:server-name req)))
    :route-params (:route-params req)
    :request-body (:body-params req)
    :jwt (:jwt req)
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

;; Used for serializing the response body only; not used for parsing the request
(def content-types
  {"application/json"
   (fn [body]
     (print-fn #(pedestal-http/json-print body)))

   "application/edn"
   (fn [body]
     ; hyperfiddle/hyperfiddle.net#38
     (print-fn #(pr body)))

   "application/transit+json"
   (fn [body]
     (fn [^OutputStream output-stream]
       (transit/write (transit/writer output-stream :json-verbose
                                      {:handlers hc-t/write-handlers}) body)
       (.flush output-stream)))

   "application/transit+msgpack"
   (fn [body]
     (fn [^OutputStream output-stream]
       (transit/write (transit/writer output-stream :msgpack) body)
       (.flush output-stream)))

   "text/html"
   (fn [body]
     (if (string? body)
       body
       (binding [clojure.pprint/*print-right-margin* 140]
         (let [body-str (as-> (with-out-str (clojure.pprint/pprint body)) $
                          (StringEscapeUtils/escapeHtml4 $)
                          (format "<html><body><pre>%s</pre></body></html>" $))]
           (print-fn #(.write *out* body-str))))))})

(def auto-content-type
  (interceptor/after
    ::auto-content-type
    (fn [{:keys [request response] :as context}]
      (let [accepts (-> (or (get-in request [:headers "accept"]) "")
                        (string/split #",")
                        (#(into #{} %)))
            accept (-> (some accepts (keys content-types))
                       #_(or "application/transit+json; charset=utf-8"))
            content-renderer (get content-types accept)]
        #_(assert content-renderer (str "invalid Accept " (get-in request [:headers "accept"])))
        (-> context
            (update-in [:response :body] (or content-renderer str))
            (update-in [:response :headers] assoc "Content-Type" accept))))))

(def promise->chan
  {:name ::promise->chan
   :leave (fn [{:keys [response] :as context}]
            (if-not (p/promise? response)
              context
              (let [channel (chan)]
                (-> response
                    (p/then (fn [response]
                              (>!! channel (assoc context :response response))))
                    (p/catch (fn [err]
                               (timbre/error err)
                               (>!! channel (assoc context :response {:status 500 :body (pr-str err)})))))
                channel)))})

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
  (let [routes (routes/build (:BUILD env))]
    (fn [req]
      (let [path (:path-info req)
            request-method (:request-method req)
            {:keys [handler route-params]} (bidi/match-route routes path :request-method request-method)]
        (timbre/debug "router:" (pr-str handler) (pr-str request-method) (pr-str path))
        (handle-route handler env (assoc-in req [:route-params] route-params))))))
