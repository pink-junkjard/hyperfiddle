(ns hyperfiddle.service.jvm.service
  (:refer-clojure :exclude [sync])
  (:require [hypercrud.transit :as hc-t]
            [hypercrud.types.Err :refer [->Err]]
            [hyperfiddle.foundation :as foundation]
            [hyperfiddle.io.bindings :refer [*subject*]]
            [hyperfiddle.io.datomic.hydrate-requests :refer [hydrate-requests]]
            [hyperfiddle.io.rpc-router :refer [decode-basis]]
            [hyperfiddle.io.sync :refer [sync]]
            [hyperfiddle.io.transact :refer [transact!]]
            [hyperfiddle.service.cookie :as cookie]
            [hyperfiddle.service.http :as http-service]
            [hyperfiddle.service.lib.jwt :as jwt]
            [hyperfiddle.service.jvm.global-basis :refer [->GlobalBasisRuntime]]
            [hyperfiddle.service.jvm.hydrate-route :refer [->HydrateRoute]]
            [hyperfiddle.service.jvm.lib.http :as http]
            [hyperfiddle.service.jvm.local-basis :refer [->LocalBasis]]
            [io.pedestal.http.body-params :as body-params]
            [io.pedestal.http.ring-middlewares :as ring-middlewares]
            [io.pedestal.http.route :refer [expand-routes]]
            [io.pedestal.interceptor.chain :refer [terminate]]
            [io.pedestal.interceptor.helpers :as interceptor]
            [promesa.core :as p]
            [ring.util.response :as ring-resp]
            [taoensso.timbre :as timbre]
            [hypercrud.browser.router :as router])
  (:import (com.auth0.jwt.exceptions JWTVerificationException)
           (java.util UUID)))


(defn e->response [e]
  ; todo there are a subset of requests that are cacheable
  {:status (or (:hyperfiddle.io/http-status-code (ex-data e)) 500)
   :headers {}                                              ; todo retry-after on 503
   :body (->Err (.getMessage e))})

(def platform-response->pedestal-response identity)

(defn build-pedestal-req-handler [platform-req-handler]
  (interceptor/handler
    (fn [req]
      (-> (platform-req-handler
            :host-env (:host-env req)
            :path-params (:path-params req)
            :request-body (:body-params req)
            :jwt (:jwt req)
            :user-id (:user-id req))
          (p/then platform-response->pedestal-response)))))

(defn http-index [req]
  (ring-resp/response "Hypercrud Server Running!"))

(defn http-hydrate-requests [req]
  (try
    (let [{:keys [body-params path-params]} req
          local-basis (decode-basis (:local-basis path-params))
          {staged-branches :staged-branches request :request} body-params
          r (hydrate-requests local-basis request staged-branches (:user-id req))]
      (ring-resp/response r))
    (catch Exception e
      (timbre/error e)
      (e->response e))))

(defn http-transact! [req]
  (try
    (-> (transact! foundation/domain-uri (:user-id req) (:body-params req))
        (ring-resp/response))
    (catch Exception e
      (timbre/error e)
      (e->response e))))

(defn http-sync [req]
  (try
    (-> (sync (:body-params req))
        (ring-resp/response))
    (catch Exception e
      (e->response e))))

(def http-global-basis
  (build-pedestal-req-handler (partial http-service/global-basis-handler ->GlobalBasisRuntime)))

(def http-local-basis
  (build-pedestal-req-handler (partial http-service/local-basis-handler ->LocalBasis)))

(def http-hydrate-route
  (build-pedestal-req-handler (partial http-service/hydrate-route-handler ->HydrateRoute)))

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

(defn routes [env]
  (let [service-root (str "/api/" (:BUILD env))]
    (expand-routes
      `[[["/" {:get [:index http-index]}]
         [~service-root {} ^:interceptors [~(body-params/body-params
                                              (body-params/default-parser-map :edn-options {:readers *data-readers*}
                                                                              :transit-options [{:handlers hc-t/read-handlers}]))
                                           http/combine-body-params
                                           http/auto-content-type
                                           ring-middlewares/cookies
                                           ~(set-host-environment (partial http-service/cloud-host-environment env))
                                           ~(with-user env)
                                           http/promise->chan]
          ["/global-basis" {:get [:global-basis http-global-basis]}]
          ["/local-basis/:global-basis/:branch/:branch-aux/*encoded-route" {:get [:local-basis-get http-local-basis]}]
          ["/local-basis/:global-basis/:branch/:branch-aux/*encoded-route" {:post [:local-basis-post http-local-basis]}]
          ["/hydrate-requests/:local-basis" {:post [:hydrate-requests http-hydrate-requests]}]
          ["/hydrate-route/:local-basis/:branch/:branch-aux/*encoded-route" {:get [:hydrate-route-get http-hydrate-route]}]
          ["/hydrate-route/:local-basis/:branch/:branch-aux/*encoded-route" {:post [:hydrate-route-post http-hydrate-route]}]
          ["/transact" {:post [:transact! http-transact!]}]
          ["/sync" {:post [:latest http-sync]}]
          ]]])))
