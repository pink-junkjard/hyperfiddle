(ns hyperfiddle.ide.service.pedestal
  (:refer-clojure :exclude [sync])
  (:require
    [contrib.base-64-url-safe :as base64-url-safe]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.ide.authenticate :as auth]
    [hyperfiddle.ide.directory :as ide-directory]           ; immoral
    [hyperfiddle.ide.domain :as ide-domain]
    [hyperfiddle.ide.service.core :as ide-service]
    [hyperfiddle.io.core :as io]
    [hyperfiddle.io.datomic.hydrate-requests :refer [hydrate-requests]]
    [hyperfiddle.io.datomic.sync :refer [sync]]
    [hyperfiddle.io.datomic.transact :refer [transact!]]
    [hyperfiddle.route :as route]
    [hyperfiddle.service.cookie :as cookie]
    [hyperfiddle.service.http]
    [hyperfiddle.service.dispatch :as dispatch]
    [hyperfiddle.service.pedestal :as hf-http]
    [promesa.core :as p]
    [taoensso.timbre :as timbre]
    [hyperfiddle.service.resolve :as R])
  (:import
    [hyperfiddle.ide.domain IdeDomain]))


(defmethod dispatch/via-domain IdeDomain [context]
  (-> context
      (hf-http/via
        (let [domain (:domain (R/from context))
              env (-> (R/from context) :config :env)
              cookie-domain (::ide-directory/ide-domain domain)]
          #_#_{:keys [cookie-name jwt-secret jwt-issuer]} (-> (get-in context [:request :domain])
                                                              domain/environment-secure :jwt)
          (hf-http/with-user-id ide-service/cookie-name
            cookie-domain
            (:AUTH0_CLIENT_SECRET env)
            (str (:AUTH0_DOMAIN env) "/"))))
      (hf-http/via
        (fn [context]
          (let [domain (get-in context [:request :domain])
                [method path] (-> context :request (select-keys [:request-method :path-info]) vals)
                route (domain/api-match-path domain path :request-method method)]

            (when-not (= (:handler route) :static-resource)
              (timbre/info "ide-router:" (pr-str (:handler route)) (pr-str method) (pr-str path)))

            (dispatch/endpoint
              (-> context
                  (assoc-in [:request :handler] (:handler route))
                  (assoc-in [:request :route-params] (:route-params route))))))
        )))

(defmethod dispatch/endpoint :hyperfiddle.ide/auth0-redirect [context]
  (hf-http/run-io context
    (fn [req]
      (let [env (-> (R/from context) :config :env)
            io (reify io/IO
                 (hydrate-requests [io local-basis partitions requests]
                   ; todo who is this executed on behalf of? system/root or the end user?
                   (p/do* (hydrate-requests (:domain req) local-basis requests partitions nil)))

                 (sync [io dbnames]
                   ; todo who is this executed on behalf of? system/root or the end user?
                   (p/do* (sync (:domain req) dbnames)))

                 (transact! [io tx-groups]
                   ; todo who is this executed on behalf of? system/root or the end user?
                   (p/do* (transact! (:domain req) nil tx-groups))))]

        (-> (auth/login env (:domain req) (R/via context R/uri-for :/) io (get-in req [:query-params :code]))
            (p/then (fn [jwt]
                      {:status 302
                       :headers {"Location" (-> (get-in req [:query-params :state]) base64-url-safe/decode)}
                       :cookies {ide-service/cookie-name (-> (get-in req [:domain ::ide-directory/ide-domain])
                                                             (cookie/jwt-options-pedestal)
                                                             (assoc :value jwt
                                                                    #_#_:expires expiration))}
                       :body ""}))))
      )))

(defmethod dispatch/endpoint :hyperfiddle.ide/logout [context]
  (assoc context
    :response {:status 302
               :headers {"Location" "/"}
               :cookies {ide-service/cookie-name (-> (get-in context [:request :domain ::ide-directory/ide-domain])
                                                     (cookie/jwt-options-pedestal)
                                                     (assoc :value (get-in context [:request :cookies ide-service/cookie-name :value])
                                                            :expires "Thu, 01 Jan 1970 00:00:00 GMT"))}
               :body ""}))

;(defn ide-routing [domain env context]
;  (let [path (get-in context [:request :path-info])
;        request-method (get-in context [:request :request-method])
;        {:keys [handler route-params]} (domain/api-match-path domain path :request-method request-method)]
;
;    ;(if (not (= handler :static-resource))
;    ;  (timbre/info "ide-router:" (pr-str handler) (pr-str request-method) (pr-str path)))
;
;    (dispatch/handle-route handler env (assoc-in context [:request :route-params] route-params))
;
;    ;(cond
;    ;  (= (some-> handler namespace) "user")
;    ;  (either/branch
;    ;    (get-in context [:request :domain ::ide-domain/user-domain+])
;    ;    (fn [e]
;    ;      (timbre/error e)
;    ;      (throw e))
;    ;    (fn [user-domain]
;    ;      (let [context (update context :request #(-> (dissoc % :jwt) ; this is brittle
;    ;                                                  (assoc :domain user-domain
;    ;                                                         :route-params route-params)))]
;    ;        (handle-route (keyword (name handler)) env context))))
;    ;
;    ;  (and (= :ssr handler)
;    ;       (= "demo" (::ide-directory/app-domain-ident domain))
;    ;       (nil? (get-in context [:request :user-id]))
;    ;       (not (string/starts-with? path "/:hyperfiddle.ide!please-login/")))
;    ;  ; todo this logic should be injected into demo domain record
;    ;  (let [inner-route (domain/url-decode domain path)
;    ;        url (domain/url-encode domain {::route/fiddle :hyperfiddle.ide/please-login
;    ;                                       ::route/datomic-args [inner-route]})]
;    ;    (-> context
;    ;        (assoc-in [:response :status] 302)
;    ;        (assoc-in [:response :headers "Location"] url)))
;    ;
;    ;  :else (handle-route handler env (assoc-in context [:request :route-params] route-params))
;      ))
