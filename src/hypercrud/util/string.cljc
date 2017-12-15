(ns hypercrud.util.string
  (:require [cats.monad.either :as either #?(:clj :refer :cljs :refer-macros) [try-either]]
            [hypercrud.compile.reader :as reader]))


(defn safe-read-edn-string [user-edn-str]
  (if user-edn-str
    ; this doesn't handle sharp-lambdas
    (try-either (reader/read-edn-string user-edn-str))
    (either/right nil)))

(def memoized-safe-read-edn-string (memoize safe-read-edn-string))
