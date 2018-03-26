(ns hyperfiddle.service.node.lib
  (:require [hypercrud.types.Err :refer [->Err]]
            [hypercrud.types.URI :refer [->URI]]
            [hyperfiddle.service.lib.jwt :as jwt]
            [hypercrud.transit :as transit]))


(defn req->service-uri [env req]
  (->URI (str (.-protocol req) "://" (.-hostname req) "/api/" (:BUILD env) "/")))

(defn- hack-buggy-express-body-text-parser [buggy-body]
  ; no-body shows up as json {} in spite of our text body parser
  (if (string? buggy-body) buggy-body nil))

(defn req->user-profile [env req]
  (some-> req .-cookies .-jwt (jwt/verify (:AUTH0_CLIENT_SECRET env))))

(defn e->response [res e]
  ; todo there are a subset of requests that are cacheable
  ; todo retry-after on 503
  (doto res
    (.status (or (:hyperfiddle.io/http-status-code (ex-data e)) 500))
    (.format #js {"application/transit+json" #(.send res (transit/encode (->Err (ex-message e))))})))
