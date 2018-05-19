(ns hyperfiddle.service.node.lib
  (:require [hypercrud.transit :as transit]
            [hypercrud.types.URI :refer [->URI]]
            [hyperfiddle.service.http :as http-service]
            [hyperfiddle.service.lib.jwt :as jwt]
            [promesa.core :as p]))


(defn req->service-uri [env req]
  (->URI (str (.-protocol req) "://" (.-hostname req) "/api/" (:BUILD env) "/")))

(defn- hack-buggy-express-body-text-parser [buggy-body]
  ; no-body shows up as json {} in spite of our text body parser
  (if (string? buggy-body) buggy-body nil))

(defn req->user-profile [env req]
  (let [verify (jwt/build-verifier env)]
    (some-> req .-cookies .-jwt verify)))

(defn platform-response->express-response [express-response platform-response]
  (doseq [[k v] (:headers platform-response)]
    (.append express-response k v))
  (doto express-response
    (.status (:status platform-response))
    (.format #js {"application/transit+json" #(.send express-response (transit/encode (:body platform-response)))})))

(defn build-node-req-handler [platform-req-handler ->Runtime]
  (fn [env req res path-params query-params]
    (let [hostname (.-hostname req)]
      (-> (platform-req-handler
            ->Runtime
            :hostname hostname
            :path-params path-params
            :request-body (some-> req .-body hack-buggy-express-body-text-parser transit/decode)
            :hyperfiddle-hostname (http-service/hyperfiddle-hostname env hostname)
            :service-uri (req->service-uri env req)
            :jwt (some-> req .-cookies .-jwt)
            :user-profile (req->user-profile env req))
          (p/then (partial platform-response->express-response res))))))
