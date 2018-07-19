(ns hyperfiddle.eval
  (:require
    [cats.core :refer [fmap]]
    [contrib.eval :as eval]))


(let [memoized-eval (memoize eval/safe-eval-string)]
  (defn read-eval-with-bindings [content & [ctx]]
    (->> (memoized-eval (str "(fn [ctx] \n" content "\n)"))
         (fmap (fn [f]
                 (f ctx))))))
