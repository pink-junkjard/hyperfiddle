(ns hypercrud.compile.macros
  (:require [hypercrud.util.core :as util]))

(defmacro str-and-code [body]
  `(with-meta ~body {:str ~(util/pprint-str body)}))
