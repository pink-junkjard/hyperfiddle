(ns hypercrud.util.string
  (:require [cats.monad.either :as either :refer-macros [try-either]]
            [cljs.reader :as reader]))

; This is not an edn reader; this is the clojurescript reader
(defn safe-read-cljs-string [code-str]
  (if code-str
    ; this doesn't handle sharp-lambdas
    (try-either (reader/read-string code-str))
    (either/right nil)))

(def memoized-safe-read-cljs-string (memoize safe-read-cljs-string))
