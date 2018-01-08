(ns hypercrud.server.service
  (:refer-clojure :exclude [sync])
  (:require [hypercrud.api.impl.hydrate-requests :refer [hydrate-requests]]
            [hypercrud.api.impl.sync :refer [sync]]
            [hypercrud.api.impl.transact :refer [transact!]]
            [hypercrud.readers]
            [hypercrud.server.util.http :as http]
            [hypercrud.transit :as hc-t]
            [hypercrud.util.base-64-url-safe :as base-64-url-safe]
            #_[io.pedestal.http.body-params :as body-params]
            #_[io.pedestal.http.route :refer [expand-routes]]
            [ring.util.response :as ring-resp]))


(defn wrap-hypercrud [m]
  m)

(defn http-index [req]
  (ring-resp/response "Hypercrud Server Running!"))

(defn http-hydrate-requests [req]
  (try
    (let [{:keys [body-params path-params]} req
          local-basis (binding [*data-readers* (merge *data-readers* {'uri #'hypercrud.types.URI/read-URI})]
                        ((comp read-string base-64-url-safe/decode) (:local-basis path-params)))
          {staged-branches :staged-branches request :request} body-params
          r (hydrate-requests local-basis request staged-branches)]
      (ring-resp/response (wrap-hypercrud r)))
    (catch Exception e
      (println "...http-hydrate; exception=" e)
      {:status 500 :headers {} :body (str e)})))

(defn http-transact! [req]
  (try
    (let [{:keys [body-params]} req
          dtx-groups body-params]
      (ring-resp/response
        (wrap-hypercrud
          (transact! dtx-groups))))
    (catch Exception e
      (println e)
      {:status 500 :headers {} :body (str e)})))

(defn http-sync [req]
  (try
    (let [dbs (:body-params req)]
      (ring-resp/response
        (wrap-hypercrud
          (sync dbs))))
    (catch Exception e
      (println e)
      {:status 500 :headers {} :body (str e)})))

#_(def routes
  (expand-routes
    `[[["/" {:get [:index http-index]}]
       ["/runtime" {} ^:interceptors [(body-params/body-params
                                    (body-params/default-parser-map :edn-options {:readers *data-readers*}
                                                                    :transit-options [{:handlers hc-t/read-handlers}]))
                                  http/combine-body-params
                                  http/auto-content-type]
        ["/hydrate-requests/:local-basis" {:post [:hydrate http-hydrate-requests]}] ; this is not cachable as it has a body
        ["/transact" {:post [:transact! http-transact!]}]
        ["/sync" {:post [:latest http-sync]}]
        ]]]))
