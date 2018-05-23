(ns hyperfiddle.ui.login
  (:require
    [contrib.base-64-url-safe :as base64-url-safe]
    [hyperfiddle.foundation :as foundation]
    [hyperfiddle.runtime :as runtime]))


(defn stateless-login-url [ctx]
  (let [{:keys [domain client-id]} (get-in ctx [:hypercrud.browser/domain :domain/environment :auth0 (:hyperfiddle-hostname ctx)])
        callback-url (str "http://" (:hostname ctx) foundation/auth0-redirect-path)
        auth-state (base64-url-safe/encode (runtime/encode-route (:peer ctx) (:route ctx)))] ; :target-route
    (str domain "/login?client=" client-id "&callbackURL=" callback-url "&state=" auth-state)))
