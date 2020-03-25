(ns hyperfiddle.ide.authenticate
  (:require
    #?(:cljs ["oauth/lib/oauth2" :refer [OAuth2]])          ; nodejs only
    [cats.core :refer [mlet return]]
    [cats.labs.promise]
    #?(:cljs [goog.object :as object])
    [hypercrud.types.DbRef :refer [->DbRef]]
    [hypercrud.types.EntityRequest :refer [map->EntityRequest]]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.foundation :as foundation]
    [hyperfiddle.io.core :as io]
    [hyperfiddle.service.jwt :as jwt]
    [promesa.core :as p]
    [taoensso.timbre :as timbre])
  #?(:clj
     (:import
       (com.auth0.client.auth AuthAPI)
       (java.util Date UUID))))


(defn fetch-id-token [env domain service-uri oauth-authorization-code]
  {:pre [domain]}
  (let [redirect-path (domain/api-path-for domain :hyperfiddle.ide/auth0-redirect)
        _ (assert redirect-path)
        redirect-uri (str service-uri redirect-path)]
    #?(:clj  (let [auth (AuthAPI. (:AUTH0_DOMAIN env) (:AUTH0_CLIENT_ID env) (:AUTH0_CLIENT_SECRET env))
                   req (.exchangeCode auth oauth-authorization-code redirect-uri)]
               ; todo async api?
               (p/do* (-> req (.execute) (.getIdToken))))
       :cljs (let [domain (:AUTH0_DOMAIN env)
                   clientID (:AUTH0_CLIENT_ID env)
                   clientSecret (:AUTH0_CLIENT_SECRET env)
                   baseSite ""
                   authorizationUrl (str domain "/authorize")
                   tokenUrl (str domain "/oauth/token")
                   customHeaders nil
                   oauth2 (OAuth2. clientID clientSecret baseSite authorizationUrl tokenUrl customHeaders)]
               (p/promise (fn [resolve! reject!]
                            (.getOAuthAccessToken
                              oauth2 oauth-authorization-code (clj->js {"grant_type" "authorization_code"
                                                                        "redirect_uri" redirect-uri})
                              (fn [e access_token refresh_token params]
                                (if e (reject! e) (resolve! (object/get params "id_token")))))))))))

(defn login [env domain service-uri io oauth-authorization-code]
  {:pre [env domain service-uri io oauth-authorization-code]}
  ;(assert false "die before login cookie set")
  (mlet [encoded-id-token (fetch-id-token env domain service-uri oauth-authorization-code)
         id-token (let [verify (jwt/build-verifier (:AUTH0_CLIENT_SECRET env)
                                                   (:AUTH0_DOMAIN env))]
                    (p/do* (verify encoded-id-token)))
         basis (io/sync io #{"$users"})
         user-record (->> (map->EntityRequest {:e [:user/sub (:sub id-token)]
                                               :db (->DbRef "$users" foundation/root-pid)
                                               :pull-exp [:db/id :user/user-id :user/created-date]})
                          (io/hydrate-one! io basis {foundation/root-pid {:is-branched true}}))
         :let [user-id (:user/user-id user-record #?(:clj (UUID/randomUUID) :cljs (random-uuid)))
               now #?(:clj (Date.) :cljs (js/Date.))]
         _ (io/transact! io {"$users" [(cond-> {:user/name (:nickname id-token)
                                                :user/sub (:sub id-token)
                                                :user/email (:email id-token)
                                                :user/picture (:picture id-token)
                                                :user/last-seen now
                                                :user/user-id user-id}
                                         (nil? (:user/created-date user-record)) (assoc :user/created-date now))]})]
    (-> (assoc id-token :user-id (str user-id))
        (jwt/sign (:AUTH0_CLIENT_SECRET env))               ; todo maybe use a different secret to sign
        (return))))
