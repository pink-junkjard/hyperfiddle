(ns hypercrud.browser.user-bindings
  (:require [cats.core :as cats :refer [mlet]]
            [cats.monad.either :as either]
            [contrib.try :refer [try-either]]
            [contrib.eval :as eval]
            [contrib.reactive :as reactive]
            [cuerdas.core :as string]
            [taoensso.timbre :as timbre]))


(let [memoized-eval-string (memoize eval/safe-eval-string)]
  (defn user-bindings [ctx]
    (let [bindings-str @(reactive/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/bindings])]
      (if (and (string? bindings-str) (not (string/blank? bindings-str)))
        (mlet [f (memoized-eval-string bindings-str)
               ctx (try-either (f ctx))]
          (if (and (not (nil? ctx)) (map? ctx))
            (cats/return ctx)
            (either/left (let [err (ex-info "user-bindings invalid" {:user-input bindings-str :user-result ctx})]
                           (timbre/error err)
                           err))))
        (either/right ctx)))))
