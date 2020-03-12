(ns hyperfiddle.service.pedestal.core
  (:require
    [clojure.string :as string]
    [promesa.core :as p]
    [taoensso.timbre :as timbre]
    [cognitect.transit :as transit]
    [hypercrud.transit :as hc-t]
    [hyperfiddle.core]                                      ; public deps
    [hyperfiddle.service.cookie :as cookie]
    [hyperfiddle.service.jwt :as jwt]
    [hypercrud.types.Err :refer [->Err]]
    [clojure.core.async :refer [chan put!]]

    [io.pedestal.http :as http]
    [io.pedestal.http.content-negotiation :as content-negotiation]
    [io.pedestal.interceptor.helpers :as interceptor]
    [io.pedestal.http.body-params :as body-params]
    [io.pedestal.http.ring-middlewares :as ring-middlewares]
    [io.pedestal.http.route]
    [io.pedestal.interceptor.chain]
    [io.pedestal.http.secure-headers :as secure-headers]
    [ring.middleware.file :as file]
    [ring.middleware.resource :as resource]
    [ring.util.response :as ring-resp])
  (:import
    [com.auth0.jwt.exceptions JWTVerificationException]
    [java.io OutputStreamWriter OutputStream]
    java.util.UUID
    org.apache.commons.lang3.StringEscapeUtils))


(defprotocol HandleRequest
  (start [H])
  (routes [H])
  (uri [H context])
  (request [H context])
  (dispatch [H context])
  ;(render [H context])
  ;(run-IO [H context])
  (serve [H context]))

(def interceptor io.pedestal.interceptor/interceptor)
(def enqueue io.pedestal.interceptor.chain/enqueue)
(def terminate io.pedestal.interceptor.chain/terminate)

(defn base-config []
  {::http/type              :jetty
   ::http/allowed-origins   {:allowed-origins (constantly true)}
   ::http/container-options {:context-configurator (fn [^org.eclipse.jetty.servlet.ServletContextHandler c]
                                                     (let [gzip-handler (org.eclipse.jetty.server.handler.gzip.GzipHandler.)]
                                                       (.setGzipHandler c gzip-handler)
                                                       (.addIncludedMethods gzip-handler (into-array ["GET" "POST"])))
                                                     c)}
   ::http/secure-headers    {:content-security-policy-settings (secure-headers/content-security-policy-header {:object-src "'none'"})
                             :content-type-settings            (secure-headers/content-type-header)}})

(defn route-via [H]
  (let [H (interceptor H)]
    (io.pedestal.http.route/expand-routes
      #{["/" :any H :route-name :index]
        ["/*" :any H :route-name :wildcard]})))

(defn via [context f]
  (enqueue context (interceptor f)))

(defn with-data-params [context]
  (-> context
      (via
        (body-params/body-params
          (body-params/default-parser-map
            :edn-options {:readers *data-readers*}
            :transit-options [{:handlers hc-t/read-handlers}])))
      (via
        (fn [context]
          (let [{:keys [json-params edn-params transit-params]} (:request context)]
            (assoc-in context [:request :body-params] (or json-params edn-params transit-params)))))))

(defn with-cookies [context]
  (enqueue context [ring-middlewares/cookies]))

(defn e->response [e]
  ; todo there are a subset of requests that are cacheable
  {:status (or (:hyperfiddle.io/http-status-code (ex-data e)) 500)
   :headers {}                                              ; todo retry-after on 503
   :body (->Err (.getMessage e))})

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
     (print-fn #(http/json-print body)))

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

(defn auto-content-type [context]
  (cond-> context
    (nil? (get-in context [:response :headers "Content-Type"]))
    (update :response coerce-to (get-in context [:request :accept :field] "text/plain"))))

(def auto-content-type-interceptor (interceptor/after ::auto-content-type auto-content-type))

(defn promise->chan [context]
  (if-not (p/promise? (:response context))
    context
    (let [channel (chan)]
      (-> (:response context)
          (p/then (fn [response]
                    (put! channel (assoc context :response response))))
          (p/catch (fn [err]
                     (timbre/error err)
                     (put! channel (assoc context :response {:status 500 :body (pr-str err)})))))
      channel)))

(defn run-io [context run]
  (-> context
      (via (content-negotiation/negotiate-content (keys content-transformers)))
      (via auto-content-type-interceptor)
      (via (fn [context]
             (promise->chan
               (try (let [response (run (:request context))]
                      (assoc context :response (cond-> response
                                                 (not (:status response)) ring-resp/response)))
                    (catch Exception e
                      (timbre/error e)
                      (e->response e))))))))

(defn- static-content [with-request mime-type-by-extension root-path context]
  (let [filename (str "/" (get-in context [:request :route-params :resource-name]))
        mime-type (or (mime-type-by-extension filename) "application/octet-stream")
        accept (get-in context [:request :headers "accept"])]
    (assoc context
      :response
      (if (or (nil? accept)                                 ; nil accept headers are ok
            (content-negotiation/best-match
              (content-negotiation/best-match-fn [mime-type])
              (content-negotiation/parse-accept-* accept)))
        (-> {:request-method (get-in context [:request :request-method])
             :path-info filename}
            (with-request root-path)
            (assoc-in [:headers "Content-Type"] mime-type))
        {:status 406
         :body "Not Acceptable"
         :headers {}}))))

(defn static-resource [mime-type-by-extension root-path context]
  (static-content resource/resource-request mime-type-by-extension root-path context))

(defn static-file [mime-type-by-extension root-path context]
  (static-content file/file-request mime-type-by-extension root-path context))

(defn with-user-id [cookie-name cookie-domain jwt-secret jwt-issuer]
  {:name ::with-user-id
   :enter (fn [context]
            (let [verify (fn [& args]
                           ; todo this is brittle
                           ; there are configurations where no auth is used
                           ; delay any exception from lack of or misconfiguration of verification until auth is used
                           (apply (jwt/build-verifier jwt-secret jwt-issuer) args))
                  jwt-cookie (get-in context [:request :cookies cookie-name :value])
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
                                             :cookies {cookie-name (-> (cookie/jwt-options-pedestal cookie-domain)
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
                          (assoc :response {:status 400 :body (->Err "Conflicting cookies and auth bearer")})))))})

(def log-request
  (interceptor/on-request
    ::log-request
    (fn [request]
      (if-not (-> request :uri (clojure.string/starts-with? "/static"))
        (timbre/info {:remote-addr (:remote-addr request)
                      :host        (:server-name request)
                      :port        (:server-port request)
                      :method      (string/upper-case (name (:request-method request)))
                      :uri         (:uri request)
                      :protocol    (:protocol request)
                      :referer     (get-in request [:headers "referer"])
                      :user-agent  (get-in request [:headers "user-agent"])}))
      request)))


;(defn domain [domain-provider]
;  {:name ::domain
;   :enter (cond
;            (instance? Domain domain-provider) #(assoc-in % [:request :domain] domain-provider)
;            (fn? domain-provider) (fn [context]
;                                    (let [channel (chan)]
;                                      (p/branch
;                                        (domain-provider (get-in context [:request :server-name]))
;                                        (fn [domain]
;                                          (put! channel (assoc-in context [:request :domain] domain)))
;                                        (fn [e]
;                                          (timbre/error e)
;                                          (let [context (-> context
;                                                            terminate
;                                                            ; todo idomatic way of handling this
;                                                            ; http://pedestal.io/reference/error-handling
;                                                            ; (io.pedestal.interceptor.chain/execute [...])
;                                                            ((:enter (content-negotiation/negotiate-content (keys content-transformers))))
;                                                            (assoc :response (e->response e))
;                                                            auto-content-type)]
;                                            (put! channel context))))
;                                      channel))
;            :else (throw (ex-info "Must supply domain value or function" {:value domain-provider})))})

;(defn build-router [env]
;  {:name ::router
;   :enter (fn [context] (service-domain/route (get-in context [:request :domain]) env context))})

;(defmethod service-domain/route :default [domain env context]
;  (let [path (get-in context [:request :path-info])
;        request-method (get-in context [:request :request-method])
;        {:keys [handler route-params]} (domain/api-match-path domain path :request-method request-method)]
;    (timbre/info "router:" (pr-str handler) (pr-str request-method) (pr-str path))
;    (handle-route handler env (assoc-in context [:request :route-params] route-params))))

;(defn routes [env domain-provider]
;  (let [interceptors [(body-params/body-params
;                        (body-params/default-parser-map :edn-options {:readers *data-readers*}
;                                                        :transit-options [{:handlers hc-t/read-handlers}]))
;                      interceptors/combine-body-params
;                      ring-middlewares/cookies
;                      (interceptors/domain domain-provider)
;                      (interceptors/build-router env)]]
;    (expand-routes
;      #{["/" :any interceptors :route-name :index]
;        ["/*" :any interceptors :route-name :wildcard]})))
