(ns hyperfiddle.ui.login
  (:require
    [contrib.base-64-url-safe :as base64-url-safe]
    [hyperfiddle.foundation :as foundation]
    [hyperfiddle.runtime :as runtime]))


(defn stateless-login-url [ctx]
  (let [{:keys [domain client-id]} (get-in ctx [:hypercrud.browser/domain :domain/environment :auth0 (:hyperfiddle-hostname ctx)])]
    (str domain "/login?"
         "client=" client-id
         "&scope=" "openid email profile"
         "&state=" (base64-url-safe/encode (runtime/encode-route (:peer ctx) (:target-route ctx)))
         "&redirect_uri=" (str "http://" (:hostname ctx) foundation/auth0-redirect-path))))
