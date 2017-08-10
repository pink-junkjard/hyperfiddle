(ns hypercrud.server.service
  (:require [hypercrud.server.api :as api]
            [hypercrud.server.db-root :as db]
            [hypercrud.server.internal :as internal]
            [hypercrud.server.util.http :as http]
            [io.pedestal.http.body-params :as body-params]
            [io.pedestal.http.ring-middlewares :as ring-middlewares]
            [io.pedestal.http.route :refer [expand-routes]]
            [ring.util.response :as ring-resp]))


(defn wrap-hypercrud [m]
  {:hypercrud m})

(defn http-hydrate
  ([req]
   (http-hydrate req (constantly true) (constantly true)))
  ([req root-write-sec root-read-sec]
   (try
     (let [{:keys [query-params body-params]} req
           root-t (if (:t query-params) (Long/parseLong (:t query-params)))
           {hctx-groups :staged-tx request :request} body-params]
       (ring-resp/response
         (wrap-hypercrud
           (api/hydrate root-read-sec root-write-sec hctx-groups request root-t))))
     (catch Exception e
       (println e)
       {:status 500 :headers {} :body (str e)}))))

(defn http-transact!
  ([req]
   (http-transact! req (constantly true)))
  ([req root-write-sec]
   (try
     (let [{:keys [body-params]} req
           htx body-params]
       (ring-resp/response
         (wrap-hypercrud
           (api/transact! root-write-sec htx))))
     (catch Exception e
       (println e)
       {:status 500 :headers {} :body (str e)}))))

(defn http-root-latest [req]
  (ring-resp/response (api/root-latest)))

(defn http-root-conn-id [req]
  (ring-resp/response db/root-id))

(defn interceptors []
  [~(body-params/body-params
      (body-params/default-parser-map :edn-options {:readers *data-readers*}
                                      :transit-options [{:handlers internal/transit-read-handlers}]))
   http/combine-body-params
   http/auto-content-type
   ring-middlewares/cookies])

(def routes
  (expand-routes
    `[[["/api" {} ^:interceptors (interceptors)
        ["/hydrate" {:post [:hydrate http-hydrate]}]
        ["/transact" {:post [:transact http-transact!]}]
        ["/latest" {:get [:latest http-root-latest]}]
        ["/root-conn-id" {:get [:root-conn-id http-root-conn-id]}]
        ]]]))
