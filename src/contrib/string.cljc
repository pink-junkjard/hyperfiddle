(ns contrib.string
  (:refer-clojure :exclude [read-string])
  (:require [cats.monad.either :as either]
            [contrib.try :refer [try-either]]
            [contrib.reader :refer [read-edn-string]]))


(defn safe-read-edn-string [user-edn-str]                   ; is this private? Should this ever be called? Isn't it slow?
  (if user-edn-str
    ; this doesn't handle sharp-lambdas
    (try-either (read-edn-string user-edn-str))
    (either/right nil)))

(def memoized-safe-read-edn-string (memoize safe-read-edn-string))
