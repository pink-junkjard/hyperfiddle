; legacy ns
(ns hypercrud.form.q-util
  (:require [cats.monad.either :as either]
            [contrib.string :refer [safe-read-edn-string]]
            [taoensso.timbre :as timbre]))


(defn ^:deprecated safe-read-string [code-str]
  (timbre/error "Warning: hypercrud.form.q-util is deprecated and will be removed in the future.  Please use contrib.string/safe-read-edn-string")
  (either/branch (safe-read-edn-string code-str)            ; memoized?
                 (constantly nil)
                 identity))
