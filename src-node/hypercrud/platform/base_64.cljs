(ns hypercrud.platform.base-64
  (:require [cljs.nodejs :as node]))


(def base64 (node/require "base-64"))

(defn encode-string [s]
  (.encode base64 s))

(defn decode-string [s]
  (.decode base64 s))
