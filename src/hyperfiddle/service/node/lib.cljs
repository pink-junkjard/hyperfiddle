(ns hyperfiddle.service.node.lib
  (:require
    [cats.monad.either :as either]
    [contrib.try :refer [try-either]]
    [hypercrud.transit :as transit]
    [hypercrud.types.Err :refer [->Err]]
    [hypercrud.types.URI :refer [->URI]]
    [hyperfiddle.service.cookie :as cookie]
    [hyperfiddle.service.http :as http-service]
    [hyperfiddle.service.lib.jwt :as jwt]
    [promesa.core :as p]
    [taoensso.timbre :as timbre]))


(defn req->service-uri [env req]
  (->URI (str (.-protocol req) "://" (.-hostname req) "/api/" (:BUILD env) "/")))

(defn- hack-buggy-express-body-text-parser [buggy-body]
  ; no-body shows up as json {} in spite of our text body parser
  (if (string? buggy-body) buggy-body nil))

(defn platform-response->express-response [express-response platform-response]
  (doseq [[k v] (:headers platform-response)]
    (.append express-response k v))
  (doto express-response
    (.status (:status platform-response))
    (.format #js {"application/transit+json" #(.send express-response (transit/encode (:body platform-response)))})))

(defn build-node-req-handler [env platform-req-handler]
  (fn [req res jwt user-id path-params query-params]
    (let [hostname (.-hostname req)]
      (-> (platform-req-handler
            :hostname hostname
            :path-params path-params
            :request-body (some-> req .-body hack-buggy-express-body-text-parser transit/decode)
            :hyperfiddle-hostname (http-service/hyperfiddle-hostname env hostname)
            :service-uri (req->service-uri env req)
            :jwt jwt
            :user-id user-id)
          (p/then (partial platform-response->express-response res))))))

(defn with-user [env]
  (let [verify (jwt/build-verifier env)]
    (fn [req res next]
      (either/branch
        ; todo support auth bearer
        (try-either (let [jwt-cookie (some-> req .-cookies .-jwt)]
                      [jwt-cookie (some-> jwt-cookie verify :user-id)]))
        (fn [e]
          (timbre/error e)
          (doto res
            (.status 401)
            (.clearCookie "jwt" (-> (http-service/hyperfiddle-hostname env (.-hostname req))
                                    (cookie/jwt-options-express)
                                    clj->js))
            (.format #js {"application/transit+json" #(.send res (transit/encode (->Err (ex-message e))))
                          ; todo flesh out a real session expiration page
                          "text/html" #(.send res "Session expired, please refresh and login")})))
        #(apply next %)))))
