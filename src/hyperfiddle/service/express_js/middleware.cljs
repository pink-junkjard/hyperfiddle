(ns hyperfiddle.service.express-js.middleware
  (:require
    [bidi.bidi :as bidi]
    [contrib.uuid :refer [read-uuid]]
    [goog.object :as object]
    [hypercrud.transit :as transit]
    [hypercrud.types.Err :refer [->Err]]
    [hyperfiddle.io.http :refer [build-routes]]
    [hyperfiddle.service.cookie :as cookie]
    [hyperfiddle.service.lib.jwt :as jwt]
    [hyperfiddle.service.http :refer [handle-route]]
    [promesa.core :as p]
    [taoensso.timbre :as timbre]))


(defn- hack-buggy-express-body-text-parser [buggy-body]
  ; no-body shows up as json {} in spite of our text body parser
  (if (string? buggy-body) buggy-body nil))

(defn platform->express-req-handler [platform-req-handler req res]
  (-> (platform-req-handler
        :host-env (object/get req "host-env")
        :route-params (object/get req "route-params")
        :request-body (some-> req .-body hack-buggy-express-body-text-parser transit/decode)
        :jwt (object/get req "jwt")
        :user-id (object/get req "user-id"))
      (p/then
        (fn [platform-response]
          (doseq [[k v] (:headers platform-response)]
            (.append res k v))
          (doto res
            (.status (:status platform-response))
            (.format #js {"application/transit+json" #(.send res (transit/encode (:body platform-response)))}))))))

(defn set-host-environment [f]
  (fn [req res next]
    (object/set req "host-env" (f (.-protocol req) (.-hostname req)))
    (next)))

(defn with-user [jwt-secret jwt-issuer]
  (let [verify (jwt/build-verifier jwt-secret jwt-issuer)]
    (fn [req res next]
      ; todo support auth bearer
      (try
        (let [jwt-cookie (some-> req .-cookies .-jwt)]
          (object/set req "jwt" jwt-cookie)
          (object/set req "user-id" (some-> jwt-cookie verify :user-id read-uuid))
          (next))
        (catch js/Error e
          (timbre/error e)
          (doto res
            (.status 401)
            (.clearCookie "jwt" (-> (:auth/root (object/get req "host-env"))
                                    (cookie/jwt-options-express)
                                    clj->js))
            (.format #js {"application/transit+json" #(.send res (transit/encode (->Err (ex-message e))))
                          ; todo flesh out a real session expiration page
                          "text/html" #(.send res "Session expired, please refresh and login")})))))))

(defn build-router [env]
  (let [routes (build-routes (:BUILD env))]
    (fn [req res]
      (let [path (.-path req)
            request-method (keyword (.toLowerCase (.-method req)))
            {:keys [handler route-params]} (bidi/match-route routes path :request-method request-method)]
        (timbre/debug "router:" (pr-str handler) (pr-str request-method) (pr-str path))
        (object/set req "route-params" route-params)
        (handle-route handler env req res route-params)))))
