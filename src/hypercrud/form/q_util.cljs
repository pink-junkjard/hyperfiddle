; legacy ns
(ns hypercrud.form.q-util
  (:require [cljs.reader :as reader]
            [taoensso.timbre :as timbre]))


; deprecated
; use hypercrud.util.string/safe-read-string
(defn safe-read-string [code-str]
  (timbre/error "Warning: hypercrud.form.q-util is deprecated and will be removed in the future.  Please use hypercrud.util.string/safe-read-string")
  (try
    (if code-str (reader/read-string code-str))             ; this doesn't handle sharp-lambdas
    (catch :default e
      ; Nothing to be done at this point -
      ; this error must be caught by the widget before it is staged.
      ;(.warn js/console "bad formula " code-str e)
      ; Happens as you type sometimes e.g. validated edn input.
      nil)))
