(ns hypercrud.browser.auto-link-txfn
  (:refer-clojure :exclude [read-string])
  #?(:cljs (:require-macros [hypercrud.browser.auto-link-txfn :refer [load-fn]]))
  (:require [cats.monad.either :as either]
            [contrib.macros :refer [str-and-code']]
            [clojure.string :as string]
            [contrib.reader :refer [read-string]]
            [contrib.string :refer [memoized-safe-read-edn-string]]
            [contrib.template :as template]
            [contrib.datomic-tx]                           ; for resource loading

            [taoensso.timbre :as timbre]))


#?(:clj
   (defmacro load-fn [filename]
     (let [code-str (-> (macroexpand `(template/load-resource ~filename))
                        (string/trim))
           code (read-string code-str)]
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
    (-> (memoized-safe-read-edn-string (str "[" (:link/path link) "]"))
        (either/branch
          (fn [e] (timbre/error e))
          (fn [path]
            (get auto-tx-fn-lookup
                 {:fe (not (nil? (first path)))
                  :c? (or (:link/create? link) false)
                  :d? (or (:link/dependent? link) false)
                  :a (not (nil? (second path)))}))))))
