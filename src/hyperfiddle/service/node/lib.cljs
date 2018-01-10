(ns hyperfiddle.service.node.lib
  (:require [hyperfiddle.service.lib.jwt :as jwt]
            [hypercrud.types.URI :refer [->URI]]
            hyperfiddle.appval.state.reducers
            [hypercrud.compile.reader :as reader]
            [hypercrud.transit :as transit]
            [hypercrud.util.base-64-url-safe :as base-64-url-safe]))


(defn req->service-uri [env req]
  (->URI (str (.-protocol req) "://" (.-hostname req) "/api/" (:BUILD env) "/")))

(defn- hack-buggy-express-body-text-parser [buggy-body]
  ; no-body shows up as json {} in spite of our text body parser
  (if (string? buggy-body) buggy-body nil))

(defn req->state-val [env req path-params query-params]
  (-> {:encoded-route (or (some-> (:double-encoded-route path-params) base-64-url-safe/decode)
                          (str "/" (:route path-params)))
       :global-basis (some-> (:global-basis path-params) base-64-url-safe/decode reader/read-edn-string) ; todo this can throw
       :local-basis (some-> (:local-basis path-params) base-64-url-safe/decode reader/read-edn-string) ; todo this can throw
       :stage (some-> req .-body hack-buggy-express-body-text-parser transit/decode)
       :user-profile (some-> req .-cookies .-jwt (jwt/verify (:AUTH0_CLIENT_SECRET env)))}
      (hyperfiddle.appval.state.reducers/root-reducer nil)))
