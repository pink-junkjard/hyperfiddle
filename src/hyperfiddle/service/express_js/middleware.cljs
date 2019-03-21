(ns hyperfiddle.service.express-js.middleware
  (:require
    [contrib.uri :refer [->URI]]
    [contrib.uuid :refer [read-uuid]]
    [goog.object :as object]
    [hypercrud.transit :as transit]
    [hypercrud.types.Err :refer [->Err]]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.service.cookie :as cookie]
    [hyperfiddle.service.domain :as service-domain]
    [hyperfiddle.service.http :as http-service :refer [handle-route]]
    [hyperfiddle.service.jwt :as jwt]
    [promesa.core :as p]
    [taoensso.timbre :as timbre]))


(defn- hack-buggy-express-body-text-parser [buggy-body]
  ; no-body shows up as json {} in spite of our text body parser
  (if (string? buggy-body) buggy-body nil))

(defn send-platform-response [express-res platform-response]
  (doseq [[k v] (:headers platform-response)]
    (.append express-res k v))
  (doto express-res
    (.status (:status platform-response))
    (.format #js {"application/transit+json" #(.send express-res (transit/encode (:body platform-response)))
                  "text/html" (fn []
                                (if (string? (:body platform-response))
                                  (.send express-res (:body platform-response))
                                  (.send express-res (pr-str (:body platform-response)))))})))

(defn service-uri [env req]
  ; we are partially trusting the request for generating a service uri which is brittle
  ; todo there should be no dependency on the request once the domain is acquired
  (->URI (str (:PUBLIC_SERVICE_HTTP_SCHEME env) "://" (.-hostname req) ":" (:PUBLIC_SERVICE_HTTP_PORT env))))

(defn platform->express-req-handler [env platform-req-handler req res]
  (-> (platform-req-handler
        :domain (object/get req "domain")
        :route-params (object/get req "route-params")
        :request-body (some-> req .-body hack-buggy-express-body-text-parser transit/decode)
        :service-uri (service-uri env req)
        :jwt (object/get req "jwt")
        :user-id (object/get req "user-id"))
      (p/then (partial send-platform-response res))))

(defn domain [domain-for-fqdn]
  (fn [req res next]
    (p/branch
      (domain-for-fqdn (.-hostname req))
      (fn [domain]
        (object/set req "domain" domain)
        (next))
      (fn [e]
        (timbre/error e)
        (->> (http-service/e->platform-response e)
             (send-platform-response res))))))

(defn with-user-id [jwt-secret jwt-issuer]
  (fn [req res next]
    (if-not (= "hyperfiddle" (domain/ident (object/get req "domain")))
      (next)
      (let [#_#_{:keys [jwt-secret jwt-issuer]} (-> (get-in context [:request :domain])
                                                    domain/environment-secure :jwt)
            verify (jwt/build-verifier jwt-secret jwt-issuer)]
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
              (.clearCookie "jwt" (-> (object/get req "domain")
                                      (get :ide-domain)
                                      cookie/jwt-options-express
                                      clj->js))
              (.format #js {"application/transit+json" #(.send res (transit/encode (->Err (ex-message e))))
                            ; todo flesh out a real session expiration page
                            "text/html" #(.send res "Session expired, please refresh and login")}))))))))

(defn build-router [env] (fn [req res] (service-domain/route (object/get req "domain") env req res)))

(defmethod service-domain/route :default [domain env req res]
  (let [path (.-path req)
        request-method (keyword (.toLowerCase (.-method req)))
        {:keys [handler route-params]} (domain/api-match-path domain path :request-method request-method)]
    (timbre/debug "router:" (pr-str handler) (pr-str request-method) (pr-str path))
    (object/set req "route-params" route-params)
    (handle-route handler env req res route-params)))
