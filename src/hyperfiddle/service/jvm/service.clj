(ns hyperfiddle.service.jvm.service
  (:refer-clojure :exclude [sync])
  (:require [contrib.base-64-url-safe :as base-64-url-safe]
            [contrib.reader :refer [read-edn-string]]
            [hypercrud.transit :as hc-t]
            [hypercrud.types.Err :refer [->Err]]
            [hyperfiddle.foundation :as foundation]
            [hyperfiddle.io.bindings :refer [*subject*]]
            [hyperfiddle.io.hydrate-requests :refer [hydrate-requests]]
            [hyperfiddle.io.sync :refer [sync]]
            [hyperfiddle.io.transact :refer [transact!]]
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
            [taoensso.timbre :as timbre]))


(defn e->response [e]
  ; todo there are a subset of requests that are cacheable
  {:status (or (:hyperfiddle.io/http-status-code (ex-data e)) 500)
   :headers {}                                              ; todo retry-after on 503
   :body (->Err (.getMessage e))})

(def platform-response->pedestal-response identity)

(defn build-pedestal-req-handler [env platform-req-handler]
  (interceptor/handler
    (fn [req]
      (let [hostname (:server-name req)]
        (-> (platform-req-handler
              :hostname hostname
              :path-params (:path-params req)
              :request-body (:body-params req)
              :hyperfiddle-hostname (http-service/hyperfiddle-hostname env hostname)
              :service-uri nil
              :jwt (:jwt req)
              :user-id (:user-id req))
            (p/then platform-response->pedestal-response))))))

(defn http-index [req]
  (ring-resp/response "Hypercrud Server Running!"))

(defn http-hydrate-requests [req]
  (try
    (let [{:keys [body-params path-params]} req
          local-basis (some-> (:local-basis path-params) base-64-url-safe/decode read-edn-string)
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

(defn http-global-basis [env]
  (build-pedestal-req-handler env (partial http-service/global-basis-handler ->GlobalBasisRuntime)))

(defn http-local-basis [env]
  (build-pedestal-req-handler env (partial http-service/local-basis-handler ->LocalBasis)))

(defn http-hydrate-route [env]
  (build-pedestal-req-handler env (partial http-service/hydrate-route-handler ->HydrateRoute)))

(defn with-user [env]
  (let [verify (jwt/build-verifier env)]
    (interceptor/before
      (fn [context]
        (let [jwt-cookie (get-in context [:request :cookies "jwt" :value])
              jwt-header (some->> (get-in context [:request :headers "authorization"])
                                  (re-find #"^Bearer (.+)$")
                                  (second))]
          (cond
            (or (nil? jwt-header)
                (= jwt-cookie jwt-header)) (-> context
                                               ; todo clear the cookie when verifciation fails
                                               (assoc-in [:request :user-id] (some-> jwt-cookie (verify)
                                                                                     :sub ; legacy
                                                                                     ))
                                               (assoc-in [:request :jwt] jwt-header))

            (nil? jwt-cookie) (-> context
                                  (assoc-in [:request :user-id] (some-> jwt-header (verify)
                                                                        :sub ; legacy
                                                                        ))
                                  (assoc-in [:request :jwt] jwt-header))

            :else (-> (terminate context)
                      (assoc :response {:status 400 :body {:message "Conflicting cookies and auth bearer"}}))))))))

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
