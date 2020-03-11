(ns hyperfiddle.service.dispatch
  (:require
    [taoensso.timbre :as timbre]
    [hiccup.core :as hiccup]
    [promesa.core :as p]
    [clojure.core.async :refer [chan put!]]

    [contrib.performance :as perf]
    [contrib.ednish :as ednish]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.service.resolve :as R]
    [hyperfiddle.io.core :as io]
    [hyperfiddle.io.global-basis :refer [global-basis]]
    [hyperfiddle.io.local-basis :refer [local-basis]]
    [hyperfiddle.io.datomic.sync :as ds]
    [hyperfiddle.io.datomic.transact :refer [transact!]]
    [hyperfiddle.io.datomic.hydrate-requests :refer [hydrate-requests]]
    [hyperfiddle.io.datomic.hydrate-route :refer [hydrate-route]]

    ; via pedestal
    [hyperfiddle.service.http :as http-service]
    [hyperfiddle.service.render :as render]
    [hyperfiddle.service.pedestal :as hf-http]
    [ring.util.response]))


; these are renamed from h.s.service-domain/route and h.s.http/handle-route
; handlers for these methods are currently distributed between here
; and hyperfiddle.ide.service.pedestal

(defmulti via-domain
  "resolves a request handler via the domain type associated with this request"
  (fn [context]
    (let [domain (:domain (R/from context))]
      (assert (not (nil? domain)))
      (type domain))))

(defmulti endpoint
  "resolves response method"
  (fn [context]
    (assert (not (nil? (-> context :request :handler))))
    (-> context :request :handler)))

(defmethod via-domain :default [context]
  (let [domain (:domain (R/from context))
        [method path] (-> context :request (select-keys [:request-method :path-info]) vals)
        route (domain/api-match-path domain path :request-method method)]

    (when-not (= (:handler route) :static-resource)
      (timbre/info "router:" (pr-str (:handler route)) (pr-str method) (pr-str path)))

    (endpoint
      (-> context
          (assoc-in [:request :handler] (:handler route))
          (assoc-in [:request :route-params] (:route-params route))))))

(defmethod endpoint :default [context]
  (let [tag (-> context :request :handler)]
    {:status 501 :body (str (pr-str tag) " not implemented")}))

(defmethod endpoint :404 [context]
  (assoc context :response (ring.util.response/not-found "Not found")))

(defmethod endpoint :405 [context]
  (assoc context :response {:status 405 :headers {} :body "Method Not Allowed"}))

(defmethod endpoint :force-refresh [context]
  (assoc context :response {:status 404 #_410 :body "Please refresh your browser"}))

(defmethod endpoint :favicon [context] (assoc context :response {:status 204}))

(defmethod endpoint :static-resource [context]
  (R/via context R/serve))

(defmethod endpoint :hydrate-requests [context]
  ;(R/via context R/run-io
  ;  (fn [r]
  ;    ...))
  (hf-http/run-io context
    (fn [req]
      (let [{:keys [body-params domain route-params]} req
            local-basis (ednish/decode-uri (:local-basis route-params))
            {partitions :partitions requests :request} body-params]
        (->> (hydrate-requests domain local-basis requests partitions (:user-id req))
          (perf/time
            (fn [total-time]
              (timbre/debugf "hydrate-requests: count %s, total time: %sms"
                (count requests) total-time))))))))  ; Todo this right?

(defmethod endpoint :sync [context]
  (hf-http/run-io context
    (fn [req]
      (ds/sync (:domain req) (:body-params req)))))

(defmethod endpoint :transact [context]
  (hf-http/run-io context
    (fn [req]
      (transact! (:domain req) (:user-id req) (:body-params req)))))

(deftype IOImpl [domain ?subject]                           ; Todo
  io/IO

  (global-basis [io]
    (global-basis io domain))

  (local-basis [io global-basis route]
    (p/resolved (local-basis io global-basis route)))

  (hydrate-requests [io local-basis partitions requests]
    (p/do* (hydrate-requests domain local-basis requests partitions ?subject)))

  (hydrate-route [io local-basis route pid partitions]
    (hydrate-route domain local-basis route pid partitions ?subject))

  (sync [io dbnames]
    (p/do* (ds/sync domain dbnames))))

(defmethod endpoint :ssr [context]
  (R/via context R/render))

(defn render [context]
  (let [env (-> (R/from context) :config :env)
        domain (:domain (R/from context))
        user-id (get-in context [:request :user-id])
        io (->IOImpl domain user-id)
        route (domain/url-decode domain
                (str (get-in context [:request :path-info]) "?" (get-in context [:request :query-string])))
        channel (chan)]

    (-> (render/render env domain io route user-id)

        (p/then (fn [{:keys [http-status-code component]}]
                  {:status  http-status-code
                   :headers {"Content-Type" "text/html"}
                   :body    (str "<!DOCTYPE html>\n" (hiccup/html (apply (first component) (rest component))))}))
        (p/catch (fn [e]
                   (timbre/error e)
                   {:status (or (:hyperfiddle.io/http-status-code (ex-data e)) 500)
                    :headers {"Content-Type" "text/html"}
                    :body (str "<h2>Fatal error:</h2><h4>" (ex-message e) "</h4>")}))
        (p/then #(put! channel (assoc context :response %))))
    channel))

(defn init-io [req run]
  (run
    :domain (:domain req)
    :route-params (:route-params req)
    :request-body (:body-params req)
    :jwt (:jwt req)
    ;:service-uri (service-uri ...)
    :user-id (:user-id req)))

(defmethod endpoint :global-basis [context]
  (hf-http/run-io context
    (fn [req]
      (init-io req (partial http-service/global-basis-handler ->IOImpl)))))

(defmethod endpoint :local-basis [context]
  (hf-http/run-io context
    (fn [req]
      (init-io req (partial http-service/local-basis-handler ->IOImpl)))))

(defmethod endpoint :hydrate-route [context]
  (hf-http/run-io context
    (fn [req]
      (init-io req (partial http-service/hydrate-route-handler ->IOImpl)))))
