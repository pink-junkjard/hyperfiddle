(ns hypercrud.server.service
  (:require [hypercrud.server.api :as api]
            [hypercrud.server.internal :as internal]
            [hypercrud.server.util.http :as http]
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
          {hctx-groups :staged-tx request :request} body-params]
      (ring-resp/response
        (wrap-hypercrud
          (api/hydrate hctx-groups request root-t))))
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

(def routes
  (expand-routes
    `[[["/" {:get [:index http-index]}]
       ["/api" {} ^:interceptors [~(body-params/body-params
                                     (body-params/default-parser-map :edn-options {:readers *data-readers*}
                                                                     :transit-options [{:handlers internal/transit-read-handlers}]))
                                  http/combine-body-params
                                  http/auto-content-type]
        ["/hydrate" {:post [:hydrate http-hydrate]}]
        ["/transact" {:post [:transact http-transact!]}]
        ]]]))
