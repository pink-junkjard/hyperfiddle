(ns contrib.base-64
  #?(:cljs (:require [goog.crypt.base64 :as base64]))
  #?(:clj
     (:import
       (java.util Base64))))


#?(:cljs
   (when (= *target* "nodejs")
     (def base64 (js/require "base-64"))))

(defn encode-string [s]
  #?(:clj  (.encodeToString (Base64/getEncoder) (.getBytes s))
     :cljs (if (= *target* "nodejs")
             (.encode base64 s)
             (base64/encodeString s))))

(defn decode-string [s]
  #?(:clj  (String. (.decode (Base64/getDecoder) (.getBytes s)))
     :cljs (if (= *target* "nodejs")
             (.decode base64 s)
             (base64/decodeString s))))
