(ns hyperfiddle.ide.service
  (:require
    [clojure.core.async :refer [chan >!!]]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.ide.domain :as ide-domain :refer [#?(:cljs IdeDomain)]]
    [hyperfiddle.service.domain :as service-domain]
    [hyperfiddle.service.http :refer [handle-route]]
    [taoensso.timbre :as timbre]
    [cats.monad.either :as either])
  #?(:clj
     (:import
       [hyperfiddle.ide.domain IdeDomain])))


; todo support express?
; todo this defmulti is overly complex
(defmethod service-domain/route IdeDomain [domain env req]
  (let [path (:path-info req)
        request-method (:request-method req)
        {:keys [handler route-params]} (domain/api-match-path domain path :request-method request-method)]
    (timbre/debug "ide-router:" (pr-str handler) (pr-str request-method) (pr-str path))
    (if (= (some-> handler namespace) "user")
      (either/branch
        (ide-domain/build-user+ (:domain req))
        (fn [e]
          (timbre/error e)
          (throw e))
        (fn [user-domain]
          (let [req (-> req
                        (dissoc :jwt)                       ; this is brittle
                        (assoc :domain user-domain))]
            (handle-route (keyword (name handler)) env (assoc-in req [:route-params] route-params)))))
      (handle-route handler env (assoc-in req [:route-params] route-params)))))
