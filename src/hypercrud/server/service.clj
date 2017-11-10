(ns hypercrud.server.service
  (:require [hypercrud.server.api :as api]
            [hypercrud.server.util.http :as http]
            [hypercrud.transit :as hc-t]
            [io.pedestal.http.body-params :as body-params]
            [io.pedestal.http.route :refer [expand-routes]]
            [ring.util.response :as ring-resp]))


(defn wrap-hypercrud [m]
  {:hypercrud m})

(defn http-index [req]
  (ring-resp/response "Hypercrud Server Running!"))

(defn http-hydrate [req]
  (try
    (let [{:keys [query-params body-params]} req
          root-t (if (:t query-params) (Long/parseLong (:t query-params)))
          {staged-branches :staged-branches request :request} body-params]
      (ring-resp/response
        (wrap-hypercrud
          (api/hydrate staged-branches request root-t))))
    (catch Exception e
      (println e)
      {:status 500 :headers {} :body (str e)})))

(defn http-transact! [req]
  (try
    (let [{:keys [body-params]} req
          dtx-groups body-params]
      (ring-resp/response
        (wrap-hypercrud
          (api/transact! dtx-groups))))
    (catch Exception e
      (println e)
      {:status 500 :headers {} :body (str e)})))

(defn http-latest [req]
  (try
    (let [dbs (:body-params req)]
      (ring-resp/response
        (wrap-hypercrud
          (api/latest dbs))))
    (catch Exception e
      (println e)
      {:status 500 :headers {} :body (str e)})))

(def routes
  (expand-routes
    `[[["/" {:get [:index http-index]}]
       ["/api" {} ^:interceptors [~(body-params/body-params
                                     (body-params/default-parser-map :edn-options {:readers *data-readers*}
                                                                     :transit-options [{:handlers hc-t/read-handlers}]))
                                  http/combine-body-params
                                  http/auto-content-type]
        ["/hydrate" {:post [:hydrate http-hydrate]}]
        ["/transact" {:post [:transact http-transact!]}]
        ]]]))
