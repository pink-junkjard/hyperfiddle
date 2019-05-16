(ns hyperfiddle.service.pedestal.core
  (:require
    [contrib.ednish :as ednish]
    [contrib.performance :as perf]
    [hypercrud.transit :as hc-t]
    [hyperfiddle.core]                                      ; public deps
    [hyperfiddle.io.datomic.hydrate-requests :refer [hydrate-requests]]
    [hyperfiddle.io.datomic.sync :as datomic-sync]
    [hyperfiddle.io.datomic.transact :refer [transact!]]
    [hyperfiddle.service.http :refer [handle-route]]
    [hyperfiddle.service.pedestal.global-basis]
    [hyperfiddle.service.pedestal.hydrate-route]
    [hyperfiddle.service.pedestal.local-basis]
    [hyperfiddle.service.pedestal.interceptors :as interceptors :refer [def-data-route e->response]]
    [io.pedestal.http.body-params :as body-params]
    [io.pedestal.http.ring-middlewares :as ring-middlewares]
    [io.pedestal.http.route :refer [expand-routes]]
    [ring.util.response :as ring-resp]
    [taoensso.timbre :as timbre]))


(defmethod handle-route :default [handler env context]
  {:status 501 :body (str (pr-str handler) " not implemented")})

(def-data-route :hydrate-requests [handler env req]
  (try
    (let [{:keys [body-params domain route-params]} req
          local-basis (ednish/decode-uri (:local-basis route-params))
          {staged-branches :staged-branches requests :request} body-params
          r (perf/time
              (fn [total-time] (timbre/debugf "hydrate-requests: count %s, has stage? %s, total time: %sms" (count requests) (not (empty? staged-branches)) total-time))
              (hydrate-requests (:datomic env) domain local-basis requests staged-branches (:user-id req)))]
      (ring-resp/response r))
    (catch Exception e
      (timbre/error e)
      (e->response e))))

(def-data-route :sync [handler env req]
  (try
    (-> (datomic-sync/sync (:datomic env) (:domain req) (:body-params req))
        (ring-resp/response))
    (catch Exception e
      (e->response e))))

(def-data-route :transact [handler env req]
  (try
    (-> (transact! (:datomic env) (:domain req) (:user-id req) (:body-params req))
        (ring-resp/response))
    (catch Exception e
      (timbre/error e)
      (e->response e))))

(defmethod handle-route :404 [handler env context]
  (assoc context :response (ring-resp/not-found "Not found")))

(defmethod handle-route :405 [handler env context]
  (assoc context :response {:status 405 :headers {} :body "Method Not Allowed"}))

(defmethod handle-route :force-refresh [handler env context]
  (assoc context :response {:status 404 #_410 :body "Please refresh your browser"}))

(defmethod handle-route :favicon [handler env context] (assoc context :response {:status 204}))

(defn routes [env domain-provider]
  (let [interceptors [(body-params/body-params
                        (body-params/default-parser-map :edn-options {:readers *data-readers*}
                                                        :transit-options [{:handlers hc-t/read-handlers}]))
                      interceptors/combine-body-params
                      ring-middlewares/cookies
                      (interceptors/domain domain-provider)
                      (interceptors/build-router env)]]
    (expand-routes
      #{["/" :any interceptors :route-name :index]
        ["/*" :any interceptors :route-name :wildcard]})))
