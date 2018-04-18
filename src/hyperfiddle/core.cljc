(ns hyperfiddle.core
  (:require [cats.core :as cats]
            [contrib.eval :as eval]
            [contrib.try :refer [try-either]]))


(def ^:dynamic ^:export *ctx* nil)

(let [memoized-eval (memoize eval/safe-eval-string)]
  (defn read-eval-with-bindings [content & [ctx]]
    (cats/bind (memoized-eval (str "(fn [ctx] (binding [hyperfiddle.core/*ctx* ctx]\n" content "\n))"))
               (fn [f] (try-either (f ctx))))))
