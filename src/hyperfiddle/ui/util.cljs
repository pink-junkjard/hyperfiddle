(ns hyperfiddle.ui.util
  (:require
    [cats.core :refer [>>=]]
    [cats.monad.either :as either]
    [contrib.eval :as eval]
    [contrib.reactive :as r]
    [contrib.reactive-debug :refer [track-cmp]]
    [contrib.string :refer [blank->nil]]
    [contrib.try :refer [try-either]]
    [contrib.ui.safe-render :refer [user-portal]]
    [hypercrud.ui.error :as ui-error]))


; defer eval until render cycle inside userportal
(let [safe-eval-string #(try-either (when % (eval/eval-string %))) ; don't actually need to safely eval, just want to memoize exceptions
      memoized-eval-string (memoize safe-eval-string)]
  (defn eval-renderer-comp [?fiddle-cljs-ns-str fiddle-renderer-str & args]
    (let [result (>>= (memoized-eval-string ?fiddle-cljs-ns-str)
                      (constantly
                        ; eval ns for the effect on the cljs namespaces
                        (memoized-eval-string fiddle-renderer-str)))]
      (either/branch
        result
        (fn [e]
          (throw e))
        (fn [f]
          (into [f] args))))))

(defn safe-reagent-f [with-error f & args]
  ^{:key (hash f)}
  [track-cmp "safe-reagent-f" user-portal with-error
   (into [f] args)])
