(ns hyperfiddle.service.jvm.service
  (:refer-clojure :exclude [sync])
  (:require
    [bidi.bidi :as bidi]
    [contrib.performance :as perf]
    [hypercrud.transit :as hc-t]
    [hypercrud.types.Err :refer [->Err]]
    [hyperfiddle.foundation :as foundation]
    [hyperfiddle.io.datomic.hydrate-requests :refer [hydrate-requests]]
    [hyperfiddle.io.http :as http]
    [hyperfiddle.io.rpc-router :refer [decode-basis]]
    [hyperfiddle.io.sync :refer [sync]]
    [hyperfiddle.io.transact :refer [transact!]]
    [hyperfiddle.service.cookie :as cookie]
    [hyperfiddle.service.http :as http-service :refer [handle-route]]
    [hyperfiddle.service.jvm.global-basis :refer [->GlobalBasisRuntime]]
    [hyperfiddle.service.jvm.hydrate-route :refer [->HydrateRoute]]
    [hyperfiddle.service.jvm.lib.http :as hf-interceptors]
    [hyperfiddle.service.jvm.local-basis :refer [->LocalBasis]]
    [hyperfiddle.service.lib.jwt :as jwt]
    [io.pedestal.http.body-params :as body-params]
    [io.pedestal.http.ring-middlewares :as ring-middlewares]
    [io.pedestal.http.route :refer [expand-routes]]
    [io.pedestal.interceptor.chain :refer [terminate]]
    [io.pedestal.interceptor.helpers :as interceptor]
    [promesa.core :as p]
    [ring.util.response :as ring-resp]
    [taoensso.timbre :as timbre])
  (:import (com.auth0.jwt.exceptions JWTVerificationException)
           (java.util UUID)))


(defn e->response [e]
  ; todo there are a subset of requests that are cacheable
  {:status (or (:hyperfiddle.io/http-status-code (ex-data e)) 500)
   :headers {}                                              ; todo retry-after on 503
   :body (->Err (.getMessage e))})

(def platform-response->pedestal-response identity)

(defn platform->pedestal-req-handler [platform-req-handler req]
  (-> (platform-req-handler
        :host-env (:host-env req)
        :route-params (:route-params req)
        :request-body (:body-params req)
        :jwt (:jwt req)
        :user-id (:user-id req))
      (p/then platform-response->pedestal-response)))

(defmethod handle-route :default [handler req]
  {:status 501 :body (str (pr-str handler) " not implemented")})

(defmethod handle-route :global-basis [handler req]
  (platform->pedestal-req-handler (partial http-service/global-basis-handler ->GlobalBasisRuntime) req))

(defmethod handle-route :hydrate-requests [handler req]
  (try
    (let [{:keys [body-params route-params]} req
          local-basis (decode-basis (:local-basis route-params))
          {staged-branches :staged-branches requests :request} body-params
          r (perf/time
              (fn [get-total-time] (timbre/debugf "hydrate-requests: count %s, has stage? %s, total time: %s" (count requests) (not (empty? staged-branches)) (get-total-time)))
              (hydrate-requests local-basis requests staged-branches (:user-id req)))]
      (ring-resp/response r))
    (catch Exception e
      (timbre/error e)
      (e->response e))))

(defmethod handle-route :hydrate-route [handler req]
  (platform->pedestal-req-handler (partial http-service/hydrate-route-handler ->HydrateRoute) req))

(defmethod handle-route :local-basis [handler req]
  (platform->pedestal-req-handler (partial http-service/local-basis-handler ->LocalBasis) req))

(defmethod handle-route :sync [handler req]
  (try
    (-> (sync (:body-params req))
        (ring-resp/response))
    (catch Exception e
      (e->response e))))

(defmethod handle-route :transact [handler req]
  (try
    (-> (transact! foundation/domain-uri (:user-id req) (:body-params req))
        (ring-resp/response))
    (catch Exception e
      (timbre/error e)
      (e->response e))))

(defmethod handle-route :404 [handler req]
  (ring-resp/not-found "Not found"))

(defmethod handle-route :405 [handler req]
  (ring-resp/not-found "Method Not Allowed"))

(defmethod handle-route :force-refresh [handler req]
  {:status 404 #_410 :body "Please refresh your browser"})

(defn set-host-environment [f]
  (interceptor/before
    (fn [context]
      (let [scheme (name (get-in context [:request :scheme]))
            hostname (get-in context [:request :server-name])]
        (assoc-in context [:request :host-env] (f scheme hostname))))))

(defn with-user [env]
  (let [verify (jwt/build-verifier env)]
    (interceptor/before
      (fn [context]
        (let [jwt-cookie (get-in context [:request :cookies "jwt" :value])
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
                                         :cookies {"jwt" (-> (get-in context [:request :host-env :auth/root])
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
                      (assoc :response {:status 400 :body (->Err "Conflicting cookies and auth bearer")}))))))))

(defn build-router [env]
  (let [routes (http/build-routes (:BUILD env))]
    (fn [req]
      (let [path (:path-info req)
            request-method (:request-method req)
            {:keys [handler route-params]} (bidi/match-route routes path :request-method request-method)]
        (timbre/debug "router:" (pr-str handler) (pr-str request-method) (pr-str path))
        (handle-route handler (assoc-in req [:route-params] route-params))))))

(defn routes [env]
  (let [interceptors [(body-params/body-params
                        (body-params/default-parser-map :edn-options {:readers *data-readers*}
                                                        :transit-options [{:handlers hc-t/read-handlers}]))
                      hf-interceptors/combine-body-params
                      hf-interceptors/auto-content-type
                      ring-middlewares/cookies
                      (set-host-environment (partial http-service/cloud-host-environment env))
                      (with-user env)
                      hf-interceptors/promise->chan
                      (build-router env)]]
    (expand-routes
      #{["/" :any interceptors :route-name :index]
        ["/*" :any interceptors :route-name :wildcard]})))
