(ns hypercrud.ui.user-attribute-renderer
  (:require [cats.monad.either :as either]
            [hypercrud.compile.eval :as eval]
            [hypercrud.ui.safe-render :refer [safe-reagent-call]]
            [hypercrud.util.core :refer [pprint-str]]))


(defn safe-eval-user-control-fn [s]
  (-> (eval/eval-str s)
      (either/branch
        (fn l [e] [:pre (pprint-str e)])
        (fn r [f]
          (when f
            (fn user-control [field props ctx]
              [safe-reagent-call f field props ctx]))))))

(defn safe-eval-user-expr [s]
  (-> (eval/eval-str s)
      (either/branch
        (fn l [e] [:pre (pprint-str e)])
        (fn r [v] v))))
