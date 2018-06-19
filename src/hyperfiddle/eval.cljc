(ns hyperfiddle.eval
  (:require
    [cats.core :refer [fmap]]
    [cats.monad.either :as either]
    [contrib.eval :as eval]
    [contrib.try :refer [try-either]]))


(let [memoized-eval (memoize eval/safe-eval-string)]
  (defn read-eval-with-bindings [content & [ctx]]
    (->> (memoized-eval (str "(fn [ctx] \n" content "\n)"))
         (fmap (fn [f]
                 (f ctx))))))
