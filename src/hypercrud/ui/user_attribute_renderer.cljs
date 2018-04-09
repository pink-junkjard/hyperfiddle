(ns hypercrud.ui.user-attribute-renderer
  (:require [cats.monad.either :as either]
            [contrib.eval :refer [eval-str]]
            [hypercrud.ui.safe-render :refer [safe-reagent-call]]
            [contrib.data :refer [pprint-str]]
            [contrib.reactive :as reactive]))


(let [safe-reagent-f (fn [with-error f & args]
                       (into [safe-reagent-call with-error f] args))]
  (defn safe-eval-user-control-fn [with-error s]
    (-> (eval-str s)
        (either/branch
          (fn l [e] [:pre (pprint-str e)])
          (fn r [f] (when f (reactive/partial safe-reagent-f with-error f)))))))
