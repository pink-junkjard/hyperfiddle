(ns hypercrud.util.string
  (:require [cats.monad.either :as either]
            [hypercrud.compile.reader :as reader]
            [hypercrud.util.non-fatal :refer [try-either]]))


(defn safe-read-edn-string [user-edn-str]
  (if user-edn-str
    ; this doesn't handle sharp-lambdas
    (try-either (reader/read-edn-string user-edn-str))
    (either/right nil)))

(def memoized-safe-read-edn-string (memoize safe-read-edn-string))
