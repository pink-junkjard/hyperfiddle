(ns hyperfiddle.service.node.lib
  (:require [hypercrud.types.URI :refer [->URI]]
            [hyperfiddle.service.lib.jwt :as jwt]))


(defn req->service-uri [env req]
  (->URI (str (.-protocol req) "://" (.-hostname req) "/api/" (:BUILD env) "/")))

(defn- hack-buggy-express-body-text-parser [buggy-body]
  ; no-body shows up as json {} in spite of our text body parser
  (if (string? buggy-body) buggy-body nil))

(defn req->user-profile [env req]
  (some-> req .-cookies .-jwt (jwt/verify (:AUTH0_CLIENT_SECRET env))))
