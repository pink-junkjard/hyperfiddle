(ns hyperfiddle.service.pedestal.service
  (:refer-clojure :exclude [sync])
  (:require
    [contrib.performance :as perf]
    [hypercrud.transit :as hc-t]
    [hyperfiddle.foundation :as foundation]
    [hyperfiddle.io.datomic.hydrate-requests :refer [hydrate-requests]]
    [hyperfiddle.io.rpc-router :refer [decode-basis]]
    [hyperfiddle.io.sync :refer [sync]]
    [hyperfiddle.io.transact :refer [transact!]]
    [hyperfiddle.service.http :as http-service :refer [handle-route]]
    [hyperfiddle.service.pedestal.global-basis]
    [hyperfiddle.service.pedestal.hydrate-route]
    [hyperfiddle.service.pedestal.local-basis]
    [hyperfiddle.service.pedestal.interceptors :as interceptors :refer [e->response]]
    [io.pedestal.http.body-params :as body-params]
    [io.pedestal.http.ring-middlewares :as ring-middlewares]
    [io.pedestal.http.route :refer [expand-routes]]
    [ring.util.response :as ring-resp]
    [taoensso.timbre :as timbre]))


(defmethod handle-route :default [handler env req]
  {:status 501 :body (str (pr-str handler) " not implemented")})

(defmethod handle-route :hydrate-requests [handler env req]
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

(defmethod handle-route :sync [handler env req]
  (try
    (-> (sync (:body-params req))
        (ring-resp/response))
    (catch Exception e
      (e->response e))))

(defmethod handle-route :transact [handler env req]
  (try
    (-> (transact! foundation/domain-uri (:user-id req) (:body-params req))
        (ring-resp/response))
    (catch Exception e
      (timbre/error e)
      (e->response e))))

(defmethod handle-route :404 [handler env req]
  (ring-resp/not-found "Not found"))

(defmethod handle-route :405 [handler env req]
  (ring-resp/not-found "Method Not Allowed"))

(defmethod handle-route :force-refresh [handler env req]
  {:status 404 #_410 :body "Please refresh your browser"})

(defn routes [env]
  (let [interceptors [(body-params/body-params
                        (body-params/default-parser-map :edn-options {:readers *data-readers*}
                                                        :transit-options [{:handlers hc-t/read-handlers}]))
                      interceptors/combine-body-params
                      interceptors/auto-content-type
                      ring-middlewares/cookies
                      interceptors/promise->chan
                      (interceptors/set-host-environment (partial http-service/cloud-host-environment env))
                      (interceptors/with-user (:AUTH0_CLIENT_SECRET env) (str (:AUTH0_DOMAIN env) "/"))
                      (interceptors/build-router env)]]
    (expand-routes
      #{["/" :any interceptors :route-name :index]
        ["/*" :any interceptors :route-name :wildcard]})))
