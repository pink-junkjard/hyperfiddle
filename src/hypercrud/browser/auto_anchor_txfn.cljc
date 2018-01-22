(ns hypercrud.browser.auto-anchor-txfn
  #?(:cljs (:require-macros [hypercrud.browser.auto-anchor-txfn :refer [load-fn]]))
  (:require [cats.monad.either :as either]
            [clojure.string :as string]
            [hypercrud.client.tx]                           ; for resource loading
            [hypercrud.compile.macros :refer [str-and-code']]
            [hypercrud.util.string :as hc-string]
            [hypercrud.util.template :as template]
            [taoensso.timbre :as timbre]
            [hypercrud.compile.reader :as reader]))


#?(:clj
   (defmacro load-fn [filename]
     (let [code-str (-> (macroexpand `(template/load-resource ~filename))
                        (string/trim))
           code (reader/read-string code-str)]
       ; cannot str-and-code' until runtime https://dev.clojure.org/jira/browse/CLJ-1206
       `(str-and-code' ~code ~code-str))))

(def auto-tx-fn-lookup
  (let [fe {{:fe true :c? false :d? true :a false} nil
            {:fe true :c? false :d? true :a true} nil
            {:fe true :c? false :d? false :a false} nil
            {:fe true :c? false :d? false :a true} nil

            {:fe true :c? true :d? true :a false} nil
            {:fe true :c? true :d? true :a true} (load-fn "auto-txfn/mt-fet-at.edn")
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
    (merge fe no-fe)))

(defn auto-txfn [link]
  ; tx-fn is not applicable if the link is not managed
  (if (or (:link/managed? link) false)
    (-> (hc-string/memoized-safe-read-edn-string (str "[" (:link/path link) "]"))
        (either/branch
          (fn [e] (timbre/error e))
          (fn [path]
            (get auto-tx-fn-lookup
                 {:fe (not (nil? (first path)))
                  :c? (or (:link/create? link) false)
                  :d? (or (:link/dependent? link) false)
                  :a (not (nil? (second path)))}))))))
