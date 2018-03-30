(ns hypercrud.ui.user-attribute-renderer
  (:require [cats.monad.either :as either]
            [contrib.eval :refer [eval-str]]
            [hypercrud.ui.safe-render :refer [safe-reagent-call]]
            [hypercrud.util.core :refer [pprint-str]]
            [hypercrud.util.reactive :as reactive]))


(let [safe-reagent-f (fn [f & args]
                       (into [safe-reagent-call f] args))]
  (defn safe-eval-user-control-fn [s]
    (-> (eval-str s)
        (either/branch
          (fn l [e] [:pre (pprint-str e)])
          (fn r [f] (when f (reactive/partial safe-reagent-f f)))))))

(defn safe-eval-user-expr [s]
  (-> (eval-str s)
      (either/branch
        (fn l [e] [:pre (pprint-str e)])
        (fn r [v] v))))
