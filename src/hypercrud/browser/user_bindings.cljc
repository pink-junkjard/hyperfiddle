(ns hypercrud.browser.user-bindings
  (:require [cats.core :as cats :refer [mlet]]
            [cats.monad.either :as either #?(:clj :refer :cljs :refer-macros) [try-either]]
            [hypercrud.compile.eval :as eval :refer [eval-str]]
            [taoensso.timbre :as timbre]))


(defn user-bindings' [link ctx]
  {:post [(not (nil? %))]}
  (if-let [code-str (eval/validate-user-code-str (:fiddle/bindings link))]
    (mlet [user-fn (eval-str code-str)
           ctx (try-either (user-fn ctx))]
      (if (and (not= nil ctx) (map? ctx))
        (cats/return ctx)
        (either/left (let [err (ex-info "user-bindings invalid" {:user-input code-str :user-result ctx})]
                       (timbre/error err)
                       err))))
    (either/right ctx)))
