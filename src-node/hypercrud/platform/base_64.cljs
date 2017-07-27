(ns hypercrud.platoform.base-64
  (:require [cljs.nodejs :as node]))


(def base64 (node/require "base-64"))

(defn encode [s]
  (.encode base64 s))

(defn decode [s]
  (.decode base64 s))
