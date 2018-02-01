(ns hypercrud.browser.user-bindings
  (:require [cats.core :as cats :refer [mlet]]
            [cats.monad.either :as either]
            [hypercrud.compile.eval :as eval]
            [hypercrud.util.non-fatal :refer [try-either]]
            [taoensso.timbre :as timbre]))


(defn user-bindings' [link ctx]
  {:post [(not (nil? %))]}
  (mlet [user-fn (eval/eval-str (:fiddle/bindings link))]
    (if user-fn
      (mlet [ctx (try-either (user-fn ctx))]
        (if (and (not= nil ctx) (map? ctx))
          (cats/return ctx)
          (either/left (let [err (ex-info "user-bindings invalid" {:user-input (:fiddle/bindings link) :user-result ctx})]
                         (timbre/error err)
                         err))))
      (cats/return ctx))))
