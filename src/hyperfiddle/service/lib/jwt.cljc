(ns hyperfiddle.service.lib.jwt
  #?(:clj
     (:import (com.auth0.jwt JWTVerifier))))


(let [#?@(:cljs [jwt (js/require "jsonwebtoken")])]
  (defn verify [token client-secret]
    #?(:clj  (-> (JWTVerifier. client-secret)
                 (.verify token))
       :cljs (some-> (.verify jwt token client-secret)
                     (js->clj :keywordize-keys true)))))
