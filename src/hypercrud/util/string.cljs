(ns hypercrud.util.string
  (:require [cljs.reader :as reader]
            [cats.monad.exception :as exception]))


(defn safe-read-string [code-str]
  (if code-str
    ; this doesn't handle sharp-lambdas
    (exception/try-on (reader/read-string code-str))
    (exception/success nil)))

(def memoized-safe-read-string (memoize safe-read-string))
