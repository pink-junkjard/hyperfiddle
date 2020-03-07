(ns hyperfiddle.service.pedestal.dispatch
  (:require
    [taoensso.timbre :as timbre]
    [hiccup.core :as hiccup]
    [promesa.core :as p]
    [clojure.core.async :refer [chan put!]]

    [contrib.performance :as perf]
    [contrib.ednish :as ednish]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.io.core :as io]
    [hyperfiddle.service.http :as http-service]
    [hyperfiddle.service.ssr :as ssr]

    ; pedestal
    [hyperfiddle.service.pedestal.core :as pedestal]
    [hyperfiddle.io.global-basis :refer [global-basis]]
    [hyperfiddle.io.local-basis :refer [local-basis]]
    [hyperfiddle.io.datomic.sync :refer [sync]]
    [hyperfiddle.io.datomic.transact :refer [transact!]]
    [hyperfiddle.io.datomic.hydrate-requests :refer [hydrate-requests]]
    [hyperfiddle.io.datomic.hydrate-route :refer [hydrate-route]]
    [ring.util.response :as ring-resp]))


(defmulti route
  "resolves a request handler via the domain type handling this request"
  (fn [domain env context]
    (type domain)))

(defmulti handle-route
  "resolves response method"
  (fn [tag env context]
    tag))


(defmethod route :default [domain env context]
  (let [path (get-in context [:request :path-info])
        request-method (get-in context [:request :request-method])
        {:keys [tag route-params]} (domain/api-match-path domain path :request-method request-method)]
    (timbre/info "router:" (pr-str tag) (pr-str request-method) (pr-str path))
    (handle-route tag env (assoc-in context [:request :route-params] route-params))))


(defmethod handle-route :default [tag env context]
  {:status 501 :body (str (pr-str tag) " not implemented")})

(defmethod handle-route :404 [tag env context]
  (assoc context :response (ring-resp/not-found "Not found")))

(defmethod handle-route :405 [tag env context]
  (assoc context :response {:status 405 :headers {} :body "Method Not Allowed"}))

(defmethod handle-route :force-refresh [tag env context]
  (assoc context :response {:status 404 #_410 :body "Please refresh your browser"}))

(defmethod handle-route :favicon [tag env context] (assoc context :response {:status 204}))

(defmethod handle-route :static-resource [tag env context]
  (pedestal/serve (:H context) context))

(defmethod handle-route :hydrate-requests [tag env context]
  (pedestal/run-io context
    (fn [req]
      (let [{:keys [body-params domain route-params]} req
            local-basis (ednish/decode-uri (:local-basis route-params))
            {partitions :partitions requests :request} body-params]
        (->> (hydrate-requests domain local-basis requests partitions (:user-id req))
          (perf/time
            (fn [total-time]
              (timbre/debugf "hydrate-requests: count %s, total time: %sms"
                (count requests) total-time))))))))  ; Todo this right?

(defmethod handle-route :sync [tag env context]
  (pedestal/run-io context
    (fn [req]
      (sync (:domain req) (:body-params req)))))

(defmethod handle-route :transact [tag env context]
  (pedestal/run-io context
    (fn [req]
      (transact! (:domain req) (:user-id req) (:body-params req)))))


(deftype IOImpl [domain ?subject]
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
    (p/do* (sync domain dbnames))))

(defmethod handle-route :ssr [tag env context]
  (let [domain (get-in context [:request :domain])
        user-id (get-in context [:request :user-id])
        io (->IOImpl domain user-id)
        route (->> (str (get-in context [:request :path-info]) "?" (get-in context [:request :query-string]))
                (domain/url-decode domain))
        channel (chan)]

    (-> (ssr/bootstrap-html-cmp env domain io route user-id)
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

(defmethod handle-route :global-basis [tag env context]
  (pedestal/run-io context
    (fn [req]
      (init-io req (partial http-service/global-basis-handler ->IOImpl)))))

(defmethod handle-route :local-basis [tag env context]
  (pedestal/run-io context
    (fn [req]
      (init-io req (partial http-service/local-basis-handler ->IOImpl)))))

(defmethod handle-route :hydrate-route [tag env context]
  (pedestal/run-io context
    (fn [req]
      (init-io req (partial http-service/hydrate-route-handler ->IOImpl)))))
