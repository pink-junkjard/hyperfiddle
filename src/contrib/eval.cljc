(ns contrib.eval
  (:require
    [cats.monad.either :as either]
    #?(:cljs [contrib.eval-cljs :as eval-cljs])
    [contrib.try$ :refer [try-either]]))


(defn eval-expr-str! [code-str]
  #?(:clj  (load-string code-str)
     :cljs (eval-cljs/eval-expr-str! code-str)))

(defn eval-expr-str!+ [code-str]
  (try-either (eval-expr-str! code-str)))

(let [memoized-eval-string (memoize eval-expr-str!+)]
  (defn ensure-fn [s]
    (if-not (string? s)
      s
      (either/branch
        (memoized-eval-string s)
        #(constantly (pr-str %))                            ; print the compile error when invoked
        (fn [user-fn]                                       ; eventually invoke this unsafe fn
          #(either/branch
             (try-either (user-fn %))
             str                                            ; print the runtime error when invoked
             identity))))))
