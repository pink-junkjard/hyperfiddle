(ns hyperfiddle.core
  (:require [contrib.eval :refer [eval-str]]))


(def ^:dynamic ^:export *ctx* nil)

(defn read-eval-with-bindings [content & [ctx]]
  (binding [*ctx* ctx]
    (eval-str content)))
