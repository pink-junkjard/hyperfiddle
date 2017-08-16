(ns hypercrud.compile.macros
  (:require [hypercrud.util.core :as util]))

(defn str-and-code' [code code-str]
  (with-meta code {:str code-str}))

(defmacro str-and-code [code]
  `(str-and-code' ~code ~(util/pprint-str code)))
