(ns hyperfiddle.service.jvm.service
  (:refer-clojure :exclude [sync])
  (:require [contrib.reader :refer [read-edn-string]]
            [hypercrud.transit :as hc-t]
            [hypercrud.types.Err :refer [->Err]]
            [contrib.base-64-url-safe :as base-64-url-safe]
            [hyperfiddle.io.hydrate-requests :refer [hydrate-requests]]
            [hyperfiddle.io.sync :refer [sync]]
            [hyperfiddle.io.transact :refer [transact!]]
            [hyperfiddle.service.http :as http-service]
            [hyperfiddle.service.lib.jwt :as jwt]
            [hyperfiddle.service.jvm.lib.http :as http]
            [hyperfiddle.service.jvm.global-basis :refer [->GlobalBasisRuntime]]
            [hyperfiddle.service.jvm.hydrate-route :refer [->HydrateRoute]]
            [hyperfiddle.service.jvm.local-basis :refer [->LocalBasis]]
            [io.pedestal.http.body-params :as body-params]
            [io.pedestal.http.ring-middlewares :as ring-middlewares]
            [io.pedestal.http.route :refer [expand-routes]]
            [io.pedestal.interceptor.helpers :as interceptor]
            [promesa.core :as p]
            [ring.util.response :as ring-resp]
            [taoensso.timbre :as timbre]))


(defn e->response [e]
  ; todo there are a subset of requests that are cacheable
  {:status (or (:hyperfiddle.io/http-status-code (ex-data e)) 500)
   :headers {}                                              ; todo retry-after on 503
   :body (->Err (.getMessage e))})

(def platform-response->pedestal-response identity)

(defn build-pedestal-req-handler [env platform-req-handler ->Runtime]
  (interceptor/handler
    (fn [req]
      (-> (platform-req-handler
            ->Runtime
            :hostname (:server-name req)
            :path-params (:path-params req)
            :request-body (:body-params req)
            :hyperfiddle-hostname (:HF_HOSTNAME env)
            :service-uri nil
            :user-profile (:user req))
          (p/then platform-response->pedestal-response)))))

(defn http-index [req]
  (ring-resp/response "Hypercrud Server Running!"))

(defn http-hydrate-requests [req]
  (try
    (let [{:keys [body-params path-params]} req
          local-basis (some-> (:local-basis path-params) base-64-url-safe/decode read-edn-string)
          {staged-branches :staged-branches request :request} body-params
          r (hydrate-requests local-basis request staged-branches)]
      (ring-resp/response r))
    (catch Exception e
      (timbre/error e)
      (e->response e))))

(defn http-transact! [req]
  (try
    (-> (transact! (:body-params req))
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

(defn http-global-basis [env]
  (build-pedestal-req-handler env http-service/global-basis-handler ->GlobalBasisRuntime))

(defn http-local-basis [env]
  (build-pedestal-req-handler env http-service/local-basis-handler ->LocalBasis))

(defn http-hydrate-route [env]
  (build-pedestal-req-handler env http-service/hydrate-route-handler ->HydrateRoute))

(defn with-user [env]
  (interceptor/on-request
    (fn [request]
      (let [user (some-> (get-in (:cookies request) ["jwt" :value])
                         (jwt/verify (:AUTH0_CLIENT_SECRET env)))]
        (assoc request :user user)))))

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
                                           ~(with-user env)
                                           http/promise->chan]
          ["/global-basis" {:get [:global-basis ~(http-global-basis env)]}]
          ["/local-basis/:global-basis/:encoded-route/:branch/:branch-aux" {:get [:local-basis-get ~(http-local-basis env)]}]
          ["/local-basis/:global-basis/:encoded-route/:branch/:branch-aux" {:post [:local-basis-post ~(http-local-basis env)]}]
          ["/hydrate-requests/:local-basis" {:post [:hydrate-requests http-hydrate-requests]}]
          ["/hydrate-route/:local-basis/:encoded-route/:branch/:branch-aux" {:get [:hydrate-route-get ~(http-hydrate-route env)]}]
          ["/hydrate-route/:local-basis/:encoded-route/:branch/:branch-aux" {:post [:hydrate-route-post ~(http-hydrate-route env)]}]
          ["/transact" {:post [:transact! http-transact!]}]
          ["/sync" {:post [:latest http-sync]}]
          ]]])))
