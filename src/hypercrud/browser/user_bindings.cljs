(ns hypercrud.browser.user-bindings
  (:require [cats.core :refer [mlet]]
            [cats.monad.exception :as exception :refer [try-on success]]
            [hypercrud.compile.eval :as eval :refer [eval-str']]))

(defn user-bindings' [link param-ctx]
  {:post [(not (nil? %))]}
  (if-let [code-str (eval/validate-user-code-str (:link/bindings link))]
    (mlet [user-fn (eval-str' code-str)]
      (try-on (user-fn param-ctx)))
    (success param-ctx)))
