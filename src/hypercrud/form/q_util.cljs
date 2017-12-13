; legacy ns
(ns hypercrud.form.q-util
  (:require [cats.monad.either :as either]
            [hypercrud.util.string :as hc-string]
            [taoensso.timbre :as timbre]))


; deprecated
; use hypercrud.util.string/safe-read-string
(defn safe-read-string [code-str]
  (timbre/error "Warning: hypercrud.form.q-util is deprecated and will be removed in the future.  Please use hypercrud.util.string/safe-read-string")
  (either/branch (hc-string/safe-read-edn-string code-str)
                 (constantly nil)
                 identity))
