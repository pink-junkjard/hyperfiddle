(ns hypercrud.browser.user-bindings
  (:require [cats.core :as cats :refer [mlet]]
            [cats.monad.either :as either :refer-macros [try-either]]
            [hypercrud.compile.eval :as eval :refer [eval-str]]))

(defn user-bindings' [link ctx]
  {:post [(not (nil? %))]}
  (if-let [code-str (eval/validate-user-code-str (:fiddle/bindings link))]
    (mlet [user-fn (eval-str code-str)
           ctx (try-either (user-fn ctx))]
      (if (and (not= nil ctx) (map? ctx))
        (cats/return ctx)
        (either/left {:message "user-bindings invalid" :data {:user-input code-str :user-result ctx}})))
    (either/right ctx)))
