(ns hypercrud.util.string
  (:require [cats.monad.either :as either :refer-macros [try-either]]
            [cljs.reader]))

(defn safe-read-edn-string [user-edn-str]
  (if user-edn-str
    ; this doesn't handle sharp-lambdas
    ; yes, cljs.reader is the edn reader, not the clojurescript reader
    (try-either (cljs.reader/read-string user-edn-str))
    (either/right nil)))

(def memoized-safe-read-edn-string (memoize safe-read-edn-string))