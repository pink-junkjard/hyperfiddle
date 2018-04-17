(ns contrib.macros
  #?(:cljs (:require-macros [contrib.macros :refer [cond-let str-and-code]]))
  (:require [contrib.string :refer [slow-pprint-str]]))


(defn str-and-code' [code code-str]
  (with-meta code {:str code-str}))

; put this in eval?
(defmacro str-and-code [code]
  `(str-and-code' ~code ~(slow-pprint-str code)))

(defmacro cond-let [& clauses]
  (when clauses
    (list 'if-let (first clauses)
          (second clauses)
          (cons `cond-let (next (next clauses))))))
