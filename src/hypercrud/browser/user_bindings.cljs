(ns hypercrud.browser.user-bindings
  (:require [cats.core :as cats :refer [mlet]]
            [cats.monad.either :as either :refer-macros [try-either]]
            [hypercrud.compile.eval :as eval :refer [eval-str']]))

(defn user-bindings' [link param-ctx]
  {:post [(not (nil? %))]}
  (if-let [code-str (eval/validate-user-code-str (:link/bindings link))]
    (mlet [user-fn (eval-str' code-str)
           param-ctx (try-either (user-fn param-ctx))]
      (if (and (not= nil param-ctx) (map? param-ctx))
        (cats/return param-ctx)
        (either/left {:message "user-bindings invalid" :data {:user-input code-str :user-result param-ctx}})))
    (either/right param-ctx)))
