(ns hypercrud.browser.user-bindings
  (:require [hypercrud.compile.eval :refer [eval-str]]))


(defn user-bindings [link param-ctx]
  (let [bindings-fn (if (empty? (:link/bindings link))
                      identity
                      (let [{f :value error :error} (eval-str (:link/bindings link))]
                        (if error
                          (fn [_] (throw error))
                          f)))]
    (try
      (bindings-fn param-ctx)
      (catch :default error
        (.warn js/console (str "error in user-bindings:\n" (pr-str error)))
        param-ctx))))
