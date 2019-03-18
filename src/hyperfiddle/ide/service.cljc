(ns hyperfiddle.ide.service
  (:require
    [cats.monad.either :as either]
    [clojure.core.async :refer [chan >!!]]
    [clojure.string :as string]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.ide.domain :as ide-domain :refer [#?(:cljs IdeDomain)]]
    [hyperfiddle.service.domain :as service-domain]
    [hyperfiddle.service.http :refer [handle-route]]
    [taoensso.timbre :as timbre])
  #?(:clj
     (:import
       [hyperfiddle.ide.domain IdeDomain])))


; todo support express?
; todo this defmulti is overly complex
(defmethod service-domain/route IdeDomain [domain env context]
  (let [path (get-in context [:request :path-info])
        request-method (get-in context [:request :request-method])
        {:keys [handler route-params]} (domain/api-match-path domain path :request-method request-method)]
    (timbre/info "ide-router:" (pr-str handler) (pr-str request-method) (pr-str path))
    (cond
      (= (some-> handler namespace) "user")
      (either/branch
        (ide-domain/build-user+ (get-in context [:request :domain]))
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
            (assoc-in [:request :status] 302)
            (assoc-in [:request :headers "Location"] url)))

      :else (handle-route handler env (assoc-in context [:request :route-params] route-params)))))
