(ns contrib.macros                                          ; put this in eval?
  #?(:cljs (:require-macros [contrib.macros :refer [str-and-code]]))
  (:require [contrib.data :as util]))


(defn str-and-code' [code code-str]
  (with-meta code {:str code-str}))

(defmacro str-and-code [code]
  `(str-and-code' ~code ~(util/pprint-str code)))
