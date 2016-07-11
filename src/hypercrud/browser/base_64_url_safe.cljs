(ns hypercrud.browser.base-64-url-safe
  (:require [goog.crypt.base64 :as base64]
            [clojure.string :as string]))


(defn encode [s]
  (-> s
      (base64/encodeString)
      (string/replace "+" "-")
      (string/replace "/" "_")
      (string/replace "=" ",")))


(defn decode [s]
  (-> s
      (string/replace "-" "+")
      (string/replace "_" "/")
      (string/replace "," "=")
      (base64/decodeString)))
