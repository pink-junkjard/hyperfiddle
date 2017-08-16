(ns hypercrud.compile.eval
  (:require [cats.monad.either :as either]
            [cljs.analyzer :as analyzer]
            [cljs.js :as cljs]
            [markdown.core]))


(def eval-str-
  (memoize (fn [code-str]
             {:pre [(not (empty? code-str))]}
             ;; Hack - we don't understand why cljs compiler doesn't handle top level forms naturally
             ;; but wrapping in identity fixes the problem
             (let [code-str' (str "(identity\n" code-str "\n)")]
               (binding [analyzer/*cljs-warning-handlers* []]
                 (cljs/eval-str (cljs/empty-state)
                                code-str'
                                nil
                                {:eval cljs/js-eval}
                                identity))))))

(defn eval-str-and-throw [code-str]
  (let [compiler-result (eval-str- code-str)]
    (if-let [error (:error compiler-result)]
      (throw error)
      (:value compiler-result))))

(def eval-
  (memoize (fn [form]
             (let [form' `(identity ~form)]
               (binding [analyzer/*cljs-warning-handlers* []]
                 (cljs/eval (cljs/empty-state)
                            form'
                            {:eval cljs/js-eval}
                            identity))))))

(defn wrap-from-compiler-result [eval-result input]
  (let [{value :value error :error} eval-result]
    (cond
      error (either/left {:message "cljs eval failed" :data {:cljs-input input :cljs-result eval-result}})
      :else (either/right value))))

(defn eval-str' [code-str]
  ;if there is a string rep in the meta, the object itself is code
  (if (:str (meta code-str))
    (either/right code-str)
    (-> code-str eval-str- (wrap-from-compiler-result code-str))))

; todo im dead code
(defn eval' [form]
  (-> form eval- (wrap-from-compiler-result form)))

(defn validate-user-code-str [code-str]
  (cond
    (:str (meta code-str)) code-str
    (and (string? code-str) (not (empty? code-str))) code-str
    (and (string? code-str) (empty? code-str)) nil          ; coerce empty to nil
    (not= nil code-str) code-str
    :else nil))