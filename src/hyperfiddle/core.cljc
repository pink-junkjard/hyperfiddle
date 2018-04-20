(ns hyperfiddle.core
  (:require [cats.monad.either :as either]
            [contrib.eval :as eval]
            [contrib.try :refer [try-either]]))


(def ^:dynamic ^:export *ctx* nil)

(let [memoized-eval (memoize eval/safe-eval-string)]
  (defn read-eval-with-bindings [content & [ctx]]
    (-> (memoized-eval (str "(fn [ctx] (binding [hyperfiddle.core/*ctx* ctx]\n" content "\n))"))
        (either/branch
          (fn [e] (throw e))
          (fn [f] (f ctx))))))
