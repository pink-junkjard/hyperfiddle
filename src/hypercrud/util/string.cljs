(ns hypercrud.util.string
  (:require [cats.monad.either :as either :refer-macros [try-either]]
            [cljs.reader :as reader]))


(defn safe-read-string [code-str]
  (if code-str
    ; this doesn't handle sharp-lambdas
    (try-either (reader/read-string code-str))
    (either/right nil)))

(def memoized-safe-read-string (memoize safe-read-string))
