(ns hypercrud.compile.eval
  (:require [cats.monad.exception :as exception]
            [cljs.analyzer :as analyzer]
            [cljs.js :as cljs]
            [markdown.core]))


(def eval-str
  (memoize (fn [code-str]
             (if-not (empty? code-str)
               ;; Hack - we don't understand why cljs compiler doesn't handle top level forms naturally
               ;; but wrapping in identity fixes the problem
               (let [code-str' (str "(identity\n" code-str "\n)")]
                 (binding [analyzer/*cljs-warning-handlers* []]
                   (cljs/eval-str (cljs/empty-state)
                                  code-str'
                                  nil
                                  {:eval cljs/js-eval}
                                  identity)))))))

(def eval
  (memoize (fn [form]
             (let [form' `(identity ~form)]
               (binding [analyzer/*cljs-warning-handlers* []]
                 (cljs/eval (cljs/empty-state)
                            form'
                            {:eval cljs/js-eval}
                            identity))))))

(defn exception-from-compiler-result [eval-result input]
  (let [{value :value error :error} eval-result]
    (cond
      error (exception/failure {:cljs-input input :cljs-result eval-result} "Failed cljs eval")
      :else (exception/success value))))

(defn eval-str' [code-str]
  ;if there is a string rep in the meta, the object itself is code
  (if (:str (meta code-str))
    (exception/success code-str)
    (-> code-str eval-str (exception-from-compiler-result code-str))))

(defn eval' [form]
  (-> form eval (exception-from-compiler-result form)))
