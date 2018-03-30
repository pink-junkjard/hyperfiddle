(ns hypercrud.browser.user-bindings
  (:require [cats.core :as cats :refer [mlet]]
            [cats.monad.either :as either]
            [contrib.try :refer [try-either]]
            [contrib.eval :refer [eval-str]]
            [contrib.reactive :as reactive]
            [taoensso.timbre :as timbre]))


(defn user-bindings' [fiddle ctx]
  {:post [(not (nil? %))]}
  (mlet [:let [bindings @(reactive/cursor fiddle [:fiddle/bindings])]
         user-fn (eval-str bindings)]
    (if user-fn
      (mlet [ctx (try-either (user-fn ctx))]
        (if (and (not= nil ctx) (map? ctx))
          (cats/return ctx)
          (either/left (let [err (ex-info "user-bindings invalid" {:user-input bindings :user-result ctx})]
                         (timbre/error err)
                         err))))
      (cats/return ctx))))
