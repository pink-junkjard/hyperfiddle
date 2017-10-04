(ns hypercrud.platform.base-64
  (:require [goog.crypt.base64 :as base64]))


(defn encode-string [s]
  (base64/encodeString s))

(defn decode-string [s]
  (base64/decodeString s))
