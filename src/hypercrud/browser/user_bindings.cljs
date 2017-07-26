(ns hypercrud.browser.user-bindings
  (:require [cats.core :as cats :refer [mlet]]
            [cats.monad.either :as either]
            [cats.monad.exception :as exception]
            [hypercrud.compile.eval :as eval :refer [eval-str']]
            [hypercrud.util.monad :refer [exception->either]]))

(defn user-bindings' [link param-ctx]
  {:post [(not (nil? %))]}
  (if-let [code-str (eval/validate-user-code-str (:link/bindings link))]
    (mlet [user-fn (eval-str' code-str)
           param-ctx (-> (exception/try-on (user-fn param-ctx)) exception->either)]
      (if (and (not= nil param-ctx) (map? param-ctx))
        (cats/return param-ctx)
        (either/left {:message "user-bindings invalid"
                      :user-input code-str
                      :user-result param-ctx})))
    (either/right param-ctx)))
