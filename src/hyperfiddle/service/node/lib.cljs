(ns hyperfiddle.service.node.lib
  (:require
    [contrib.uuid :refer [read-uuid]]
    [goog.object :as object]
    [hypercrud.transit :as transit]
    [hypercrud.types.Err :refer [->Err]]
    [hyperfiddle.service.cookie :as cookie]
    [hyperfiddle.service.lib.jwt :as jwt]
    [promesa.core :as p]
    [taoensso.timbre :as timbre]))


(defn- hack-buggy-express-body-text-parser [buggy-body]
  ; no-body shows up as json {} in spite of our text body parser
  (if (string? buggy-body) buggy-body nil))

(defn platform-response->express-response [express-response platform-response]
  (doseq [[k v] (:headers platform-response)]
    (.append express-response k v))
  (doto express-response
    (.status (:status platform-response))
    (.format #js {"application/transit+json" #(.send express-response (transit/encode (:body platform-response)))})))

(defn platform->express-req-handler [platform-req-handler req res]
  (-> (platform-req-handler
        :host-env (object/get req "host-env")
        :route-params (object/get req "route-params")
        :request-body (some-> req .-body hack-buggy-express-body-text-parser transit/decode)
        :jwt (object/get req "jwt")
        :user-id (object/get req "user-id"))
      (p/then (partial platform-response->express-response res))))

(defn with-user [env]
  (let [verify (jwt/build-verifier env)]
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
