(ns hyperfiddle.service.jvm.service
  (:refer-clojure :exclude [sync])
  (:require [cats.core :refer [mlet return]]
            [hypercrud.browser.routing :as routing]
            [hypercrud.compile.reader :as reader]
            [hypercrud.transit :as hc-t]
            [hypercrud.util.base-64-url-safe :as base-64-url-safe]
            [hypercrud.util.reactive :as reactive]
            [hyperfiddle.appval.state.reducers :as reducers]
            [hyperfiddle.io.domain :refer [fetch-domain!]]
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
      (println "...http-hydrate; exception=" e)
      {:status 500 :headers {} :body (str e)})))

(defn http-transact! [req]
  (try
    (let [{:keys [body-params]} req
          dtx-groups body-params]
      (ring-resp/response
        (transact! dtx-groups)))
    (catch Exception e
      (println e)
      {:status 500 :headers {} :body (str e)})))

(defn http-sync [req]
  (try
    (let [dbs (:body-params req)]
      (ring-resp/response
        (sync dbs)))
    (catch Exception e
      (println e)
      {:status 500 :headers {} :body (str e)})))

(defn http-global-basis [env]
  (interceptor/handler
    (fn [req]
      (try
        (let [hostname (:server-name req)
              state-val (-> {:user-profile (:user req)}
                            (reducers/root-reducer nil))
              rt (->GlobalBasisRuntime (:HF_HOSTNAME env) hostname (reactive/atom state-val))]
          (-> (runtime/global-basis rt)
              (p/then (fn [global-basis]
                        {:status 200
                         :headers {"Cache-Control" "max-age=0"}
                         :body global-basis}))))
        (catch Exception e
          ; todo this try catch should be an interceptor
          (timbre/error e)
          {:status 500 :headers {} :body (str e)})))))

(defn http-local-basis [env]
  (interceptor/handler
    (fn [{:keys [path-params] :as req}]
      (try
        (let [hostname (:server-name req)
              global-basis (-> (:global-basis path-params) base-64-url-safe/decode reader/read-edn-string) ; todo this can throw
              state-val (-> {:global-basis global-basis
                             :user-profile (:user req)}
                            (reducers/root-reducer nil))
              route (routing/decode (str "/" (:encoded-route path-params)))
              foo (some-> (:foo path-params) base-64-url-safe/decode reader/read-edn-string)
              branch (some-> (:branch path-params) base-64-url-safe/decode reader/read-edn-string)]
          (-> (mlet [domain (fetch-domain! hostname (:HF_HOSTNAME env) (:domain global-basis) nil #_"WHY NO PASS STAGE?????")
                     :let [rt (->LocalBasis (:HF_HOSTNAME env) hostname domain foo nil (reactive/atom state-val))]
                     local-basis (runtime/local-basis rt global-basis route branch)]
                (return
                  {:status 200
                   :headers {"Cache-Control" "max-age=31536000"}
                   :body local-basis}))
              (p/catch (fn [e] {:status 500 :headers {} :body (str e)}))))
        (catch Exception e
          ; todo this try catch should be an interceptor
          (timbre/error e)
          {:status 500 :headers {} :body (str e)})))))

(defn http-hydrate-route [env]
  (interceptor/handler
    (fn [{:keys [body-params path-params] :as req}]
      (try
        (let [hostname (:server-name req)
              local-basis (-> (:local-basis path-params) base-64-url-safe/decode reader/read-edn-string) ; todo this can throw
              route (routing/decode (str "/" (:encoded-route path-params)))
              foo (some-> (:foo path-params) base-64-url-safe/decode reader/read-edn-string)
              target-repo (some-> (:target-repo path-params) base-64-url-safe/decode reader/read-edn-string)
              branch (some-> (:branch path-params) base-64-url-safe/decode reader/read-edn-string)
              domain-basis (first (filter (fn [[k v]] (= #uri "datomic:free://datomic:4334/domains" k)) local-basis))
              state-val (-> {:local-basis local-basis
                             :stage body-params
                             :user-profile (:user req)}
                            (reducers/root-reducer nil))]
          (mlet [domain (fetch-domain! hostname (:HF_HOSTNAME env) domain-basis body-params)
                 :let [rt (->HydrateRoute (:HF_HOSTNAME env) hostname domain foo target-repo (reactive/atom state-val))]
                 data (runtime/hydrate-route rt local-basis route branch (:stage state-val))]
            (return
              {:status 200
               :headers {"Cache-Control" "max-age=31536000"} ; todo max-age=0 if POST
               :body data})))
        (catch Exception e
          ; todo this try catch should be an interceptor
          (timbre/error e)
          ; todo caching on errors, there are a subset of requests that are actually permanently cacheable
          {:status 500 :headers {} :body (str e)})))))

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
          ["/local-basis/:global-basis/:encoded-route/:foo" {:get [:local-basis-get ~(http-local-basis env)]}]
          ["/local-basis/:global-basis/:encoded-route/:foo" {:post [:local-basis-post ~(http-local-basis env)]}]
          ["/hydrate-requests/:local-basis" {:post [:hydrate-requests http-hydrate-requests]}]
          ["/hydrate-route/:local-basis/:encoded-route/:foo/:target-repo/:branch" {:get [:hydrate-route-get ~(http-hydrate-route env)]}]
          ["/hydrate-route/:local-basis/:encoded-route/:foo/:target-repo/:branch" {:post [:hydrate-route-post ~(http-hydrate-route env)]}]
          ["/transact" {:post [:transact! http-transact!]}]
          ["/sync" {:post [:latest http-sync]}]
          ]]])))
