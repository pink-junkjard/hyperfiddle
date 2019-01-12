(ns hyperfiddle.ide.authenticate
  (:require
    ["oauth/lib/oauth2" :refer [OAuth2]]
    [bidi.bidi :as bidi]
    [cats.core :refer [mlet return]]
    [cats.labs.promise]
    [contrib.base-64-url-safe :as base64-url-safe]
    [goog.object :as object]
    [hypercrud.types.DbRef :refer [->DbRef]]
    [hypercrud.types.EntityRequest :refer [map->EntityRequest]]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.io.core :as io]
    [hyperfiddle.io.routes :as routes]
    [hyperfiddle.service.cookie :as cookie]
    [hyperfiddle.service.jwt :as jwt]
    [promesa.core :as p]))


(defn oauth2 [env]
  (let [domain (:AUTH0_DOMAIN env)
        clientID (:AUTH0_CLIENT_ID env)
        clientSecret (:AUTH0_CLIENT_SECRET env)
        baseSite ""
        authorizationUrl (str domain "/authorize")
        tokenUrl (str domain "/oauth/token")
        customHeaders nil]
    (OAuth2. clientID clientSecret baseSite authorizationUrl tokenUrl customHeaders)))

(defn fetch-id-token [env service-uri oauth-authorization-code]
  (let [redirect-uri (str service-uri (bidi/path-for (routes/build (:BUILD env)) :auth0-redirect))]
    (p/promise (fn [resolve! reject!]
                 (.getOAuthAccessToken
                   (oauth2 env) oauth-authorization-code #js {"grant_type" "authorization_code"
                                                              "redirect_uri" redirect-uri}
                   (fn [e access_token refresh_token params]
                     (if e (reject! e) (resolve! (object/get params "id_token")))))))))

(defn login [env service-uri io oauth-authorization-code]
  (mlet [encoded-id-token (fetch-id-token env service-uri oauth-authorization-code)
         id-token ((jwt/build-verifier (:AUTH0_CLIENT_SECRET env) (str (:AUTH0_DOMAIN env) "/")) encoded-id-token)
         basis (io/sync io #{"$users"})
         user-record (->> (map->EntityRequest {:e [:user/sub (:sub id-token)]
                                               :db (->DbRef "$users" nil)
                                               :pull-exp [:db/id :user/user-id :user/created-date]})
                          (io/hydrate-one! io basis nil))
         :let [user-id (:user/user-id user-record (random-uuid))]
         _ (io/transact! io {"$users" [(cond-> {:user/name (:nickname id-token)
                                                :user/sub (:sub id-token)
                                                :user/email (:email id-token)
                                                :user/picture (:picture id-token)
                                                :user/last-seen (js/Date. (js/Date.now))
                                                :user/user-id user-id}
                                         (nil? (:user/created-date user-record)) (assoc :user/created-date (js/Date.)))]})]
    (-> (assoc id-token :user-id (str user-id))
        (jwt/sign (:AUTH0_CLIENT_SECRET env))               ; todo maybe use a different secret to sign
        (return))))
