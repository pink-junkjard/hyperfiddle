(ns hypercrud.browser.auto-anchor-txfn
  (:require-macros [hypercrud.util.template :as template])
  (:require [cats.monad.either :as either]
            [clojure.string :as string]
            [hypercrud.compile.eval :as eval]
            [hypercrud.compile.macros :refer [str-and-code']]
            [hypercrud.util.string :as hc-string]))


(def auto-tx-fn-lookup
  (letfn [(process-resource [code-str]
            (let [code-str (string/trim code-str)]
              (str-and-code' (eval/eval-str-and-throw code-str) code-str)))]
    (let [fe-dep-attr (process-resource (template/load-resource "auto-txfn/mt-fet-at.edn"))

          fe {{:fe true :c? false :d? true :a false} nil
              {:fe true :c? false :d? true :a true} nil
              {:fe true :c? false :d? false :a false} nil
              {:fe true :c? false :d? false :a true} nil

              {:fe true :c? true :d? true :a false} nil
              {:fe true :c? true :d? true :a true} fe-dep-attr
              {:fe true :c? true :d? false :a false} nil
              {:fe true :c? true :d? false :a true} nil}

          ; no fe = index or relation links
          no-fe {{:fe false :c? false :d? true :a false} nil
                 {:fe false :c? false :d? true :a true} nil
                 {:fe false :c? false :d? false :a false} nil
                 {:fe false :c? false :d? false :a true} nil

                 {:fe false :c? true :d? true :a false} nil
                 {:fe false :c? true :d? true :a true} nil
                 {:fe false :c? true :d? false :a false} nil
                 {:fe false :c? true :d? false :a true} nil}]
      (merge fe no-fe))))

(defn auto-txfn [anchor]
  ; tx-fn is not applicable if the anchor is not managed
  (if (or (:anchor/managed? anchor) false)
    (-> (hc-string/memoized-safe-read-edn-string (str "[" (:link/path anchor) "]"))
        (either/branch
          (fn [e]
            (js/console.error (pr-str e))
            nil)
          (fn [path]
            (get auto-tx-fn-lookup
                 {:fe (not (nil? (first path)))
                  :c? (or (:anchor/create? anchor) false)
                  :d? (or (:anchor/repeating? anchor) false)
                  :a (not (nil? (second path)))}))))))
