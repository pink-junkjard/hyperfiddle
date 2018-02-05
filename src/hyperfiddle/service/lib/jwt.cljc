(ns hyperfiddle.service.lib.jwt
  #?(:clj
     (:require [clojure.walk :as walk]))
  #?(:clj
     (:import (com.auth0.jwt JWTVerifier))))


(let [#?@(:cljs [jwt (js/require "jsonwebtoken")])]
  (defn verify [token client-secret]
    #?(:clj  (->> (-> (JWTVerifier. client-secret)
                      (.verify token))
                  (into {})
                  (walk/keywordize-keys))
       :cljs (some-> (.verify jwt token client-secret)
                     (js->clj :keywordize-keys true)))))
