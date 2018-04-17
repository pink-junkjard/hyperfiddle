(ns hyperfiddle.core
  (:require [cats.core :as cats]
            [contrib.eval :as eval]
            [contrib.try :refer [try-either]]))


(def ^:dynamic ^:export *ctx* nil)

(let [safe-eval #(try-either (eval/eval-string (str "(fn [ctx] (binding [hyperfiddle.core/*ctx* ctx]\n" % "\n))")))
      memoized-eval (memoize safe-eval)]
  (defn read-eval-with-bindings [content & [ctx]]
    (cats/bind (memoized-eval content)
               (fn [f] (try-either (f ctx))))))
