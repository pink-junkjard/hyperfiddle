(ns hypercrud.compile.eval
  (:require [cljs.analyzer :as analyzer]
            [cljs.js :as cljs]
            [markdown.core]))


(def eval-str
  (memoize (fn [code-str]
             ;; Hack - we don't understand why cljs compiler doesn't handle top level forms naturally
             ;; but wrapping in identity fixes the problem
             (let [code-str' (str "(identity\n" code-str "\n)")]
               (binding [analyzer/*cljs-warning-handlers* []]
                 (cljs/eval-str (cljs/empty-state)
                                code-str'
                                nil
                                {:eval cljs/js-eval}
                                identity))))))

(def eval
  (memoize (fn [form]
             (let [form' `(identity ~form)]
               (binding [analyzer/*cljs-warning-handlers* []]
                 (cljs/eval (cljs/empty-state)
                            form'
                            {:eval cljs/js-eval}
                            identity))))))
