(ns hyperfiddle.service.jvm.service
  (:refer-clojure :exclude [sync])
  (:require [hypercrud.browser.routing :as routing]
            [hypercrud.compile.reader :as reader]
            [hypercrud.transit :as hc-t]
            [hypercrud.types.Err :refer [->Err]]
            [hypercrud.util.base-64-url-safe :as base-64-url-safe]
            [hypercrud.util.reactive :as reactive]
            [hyperfiddle.appval.state.reducers :as reducers]
            [hyperfiddle.foundation.actions :as foundation-actions]
            [hyperfiddle.io.hydrate-requests :refer [hydrate-requests]]
            [hyperfiddle.io.sync :refer [sync]]
            [hyperfiddle.io.transact :refer [transact!]]
            [hyperfiddle.runtime :as runtime]
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

(defn http-index [req]
  (ring-resp/response "Hypercrud Server Running!"))

(defn http-hydrate-requests [req]
  (try
    (let [{:keys [body-params path-params]} req
          local-basis (some-> (:local-basis path-params) base-64-url-safe/decode reader/read-edn-string)
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
  (interceptor/handler
    (fn [req]
      (let [hostname (:server-name req)
            state-val (-> {:user-profile (:user req)}
                          (reducers/root-reducer nil))
            rt (->GlobalBasisRuntime (:HF_HOSTNAME env) hostname (reactive/atom state-val))]
        (-> (runtime/global-basis rt)
            (p/then (fn [global-basis]
                      {:status 200
                       :headers {"Cache-Control" "max-age=0"}
                       :body global-basis}))
            (p/catch (fn [e]
                       (timbre/error e)
                       (e->response e))))))))

(defn http-local-basis [env]
  (interceptor/handler
    (fn [{:keys [path-params] :as req}]
      (try
        (let [hostname (:server-name req)
              global-basis (-> (:global-basis path-params) base-64-url-safe/decode reader/read-edn-string) ; todo this can throw
              route (-> (:encoded-route path-params) base-64-url-safe/decode reader/read-edn-string)
              _  (when-let [e (routing/invalid-route? route)] (throw e))
              branch (some-> (:branch path-params) base-64-url-safe/decode reader/read-edn-string)
              branch-aux (some-> (:branch-aux path-params) base-64-url-safe/decode reader/read-edn-string)
              initial-state {:user-profile (:user req)
                             ::runtime/global-basis global-basis
                             ::runtime/partitions {branch {:route route
                                                           :hyperfiddle.runtime/branch-aux branch-aux}}}
              rt (->LocalBasis (:HF_HOSTNAME env) hostname
                               (reactive/atom (reducers/root-reducer initial-state nil))
                               reducers/root-reducer)]
          (-> (foundation-actions/refresh-domain rt (partial runtime/dispatch! rt) #(deref (runtime/state rt)))
              (p/then (fn [_] (runtime/local-basis rt global-basis route branch branch-aux)))
              (p/then (fn [local-basis]
                        {:status 200
                         :headers {"Cache-Control" "max-age=31536000"}
                         :body local-basis}))
              (p/catch (fn [e]
                         (timbre/error e)
                         (e->response e)))))
        (catch Exception e
          (timbre/error e)
          (e->response e))))))

(defn http-hydrate-route [env]
  (interceptor/handler
    (fn [{:keys [body-params path-params] :as req}]
      (try
        (let [hostname (:server-name req)
              local-basis (-> (:local-basis path-params) base-64-url-safe/decode reader/read-edn-string) ; todo this can throw
              route (-> (:encoded-route path-params) base-64-url-safe/decode reader/read-edn-string)
              _  (when-let [e (routing/invalid-route? route)] (throw e))
              branch (some-> (:branch path-params) base-64-url-safe/decode reader/read-edn-string)
              branch-aux (some-> (:branch-aux path-params) base-64-url-safe/decode reader/read-edn-string)
              initial-state (-> {:user-profile (:user req)
                                 :stage body-params
                                 ; should this be constructed with reducers?
                                 ; why dont we need to preheat the tempid lookups here for parent branches?
                                 ::runtime/partitions {branch {:local-basis local-basis
                                                               :route route
                                                               :hyperfiddle.runtime/branch-aux branch-aux}}})
              rt (->HydrateRoute (:HF_HOSTNAME env) hostname
                                 (reactive/atom (reducers/root-reducer initial-state nil))
                                 reducers/root-reducer)]
          (-> (foundation-actions/refresh-domain rt (partial runtime/dispatch! rt) #(deref (runtime/state rt)))
              (p/then (fn [_] (runtime/hydrate-route rt local-basis route branch branch-aux (:stage initial-state))))
              (p/then (fn [data]
                        {:status 200
                         :headers {"Cache-Control" "max-age=31536000"} ; todo max-age=0 if POST
                         :body data}))
              (p/catch (fn [e]
                         (timbre/error e)
                         (e->response e)))))
        (catch Exception e
          (timbre/error e)
          (e->response e))))))

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
