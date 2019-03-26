(ns hyperfiddle.ide.service.pedestal
  (:refer-clojure :exclude [sync])
  (:require
    [cats.monad.either :as either]
    [clojure.string :as string]
    [contrib.base-64-url-safe :as base64-url-safe]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.ide.authenticate :as auth]
    [hyperfiddle.ide.domain :as ide-domain]
    [hyperfiddle.ide.service.core :as ide-service]
    [hyperfiddle.io.core :as io]
    [hyperfiddle.io.datomic.hydrate-requests :refer [hydrate-requests]]
    [hyperfiddle.io.datomic.sync :refer [sync]]
    [hyperfiddle.io.datomic.transact :refer [transact!]]
    [hyperfiddle.service.cookie :as cookie]
    [hyperfiddle.service.domain :as service-domain]
    [hyperfiddle.service.http :refer [handle-route]]
    [hyperfiddle.service.pedestal.interceptors :as interceptors :refer [def-data-route]]
    [io.pedestal.interceptor :as interceptor]
    [io.pedestal.interceptor.chain :refer [enqueue]]
    [promesa.core :as p]
    [taoensso.timbre :as timbre])
  (:import
    [hyperfiddle.ide.domain IdeDomain]))


(def-data-route :hyperfiddle.ide/auth0-redirect [handler env req]
  (let [io (reify io/IO
             (hydrate-requests [io local-basis staged-branches requests]
               ; todo who is this executed on behalf of? system/root or the end user?
               (p/do* (hydrate-requests (:domain req) local-basis requests staged-branches nil)))

             (sync [io dbnames]
               ; todo who is this executed on behalf of? system/root or the end user?
               (p/do* (sync (:domain req) dbnames)))

             (transact! [io tx-groups]
               ; todo who is this executed on behalf of? system/root or the end user?
               (p/do* (transact! (:domain req) nil tx-groups))))]
    (-> (auth/login env (:domain req) (interceptors/service-uri env req) io (get-in req [:query-params :code]))
        (p/then (fn [jwt]
                  {:status 302
                   :headers {"Location" (-> (get-in req [:query-params :state]) base64-url-safe/decode)}
                   :cookies {ide-service/cookie-name (-> (get-in req [:domain :ide-domain])
                                                         (cookie/jwt-options-pedestal)
                                                         (assoc :value jwt
                                                                #_#_:expires expiration))}
                   :body ""})))))

(defmethod handle-route :hyperfiddle.ide/logout [handler env context]
  (assoc context
    :response {:status 302
               :headers {"Location" "/"}
               :cookies {ide-service/cookie-name (-> (get-in context [:request :domain :ide-domain])
                                                     (cookie/jwt-options-pedestal)
                                                     (assoc :value (get-in context [:request :cookies ide-service/cookie-name :value])
                                                            :expires "Thu, 01 Jan 1970 00:00:00 GMT"))}
               :body ""}))

(defn ide-routing [domain env context]
  (let [path (get-in context [:request :path-info])
        request-method (get-in context [:request :request-method])
        {:keys [handler route-params]} (domain/api-match-path domain path :request-method request-method)]
    (timbre/info "ide-router:" (pr-str handler) (pr-str request-method) (pr-str path))
    (cond
      (= (some-> handler namespace) "user")
      (either/branch
        (get-in context [:request :domain ::ide-domain/user-domain+])
        (fn [e]
          (timbre/error e)
          (throw e))
        (fn [user-domain]
          (let [context (update context :request #(-> (dissoc % :jwt) ; this is brittle
                                                      (assoc :domain user-domain
                                                             :route-params route-params)))]
            (handle-route (keyword (name handler)) env context))))

      (and (= :ssr handler)
           (= "demo" (:app-domain-ident domain))
           (nil? (get-in context [:request :user-id]))
           (not (string/starts-with? path "/:hyperfiddle.ide!please-login/")))
      ; todo this logic should be injected into demo domain record
      (let [inner-route (domain/url-decode domain path)
            url (domain/url-encode domain [:hyperfiddle.ide/please-login inner-route])]
        (-> context
            (assoc-in [:response :status] 302)
            (assoc-in [:response :headers "Location"] url)))

      :else (handle-route handler env (assoc-in context [:request :route-params] route-params)))))

(defmethod service-domain/route IdeDomain [domain env context]
  (enqueue context [(let [#_#_{:keys [cookie-name jwt-secret jwt-issuer]} (-> (get-in context [:request :domain])
                                                                              domain/environment-secure :jwt)
                          cookie-domain (:ide-domain domain)]
                      (interceptor/interceptor
                        (interceptors/with-user-id ide-service/cookie-name cookie-domain (:AUTH0_CLIENT_SECRET env) (str (:AUTH0_DOMAIN env) "/"))))
                    (interceptor/interceptor
                      {:name :ide-routing
                       :enter (partial ide-routing domain env)})]))
