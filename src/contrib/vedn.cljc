(ns contrib.vedn
  (:refer-clojure :exclude [read-string])
  #?(:cljs (:require-macros [contrib.vedn :refer [load-vedn-from-file]]))
  (:require [clojure.string :as string]
            [contrib.macros :refer [str-and-code']]
            [contrib.reader :refer [read-string]]
            [contrib.template :as template]))


(def vedn-delimiter "(?m)^==")

(defn split-vedn-list [data-str]
  (string/split data-str (re-pattern vedn-delimiter)))

; a format, a map written in list form, delimited by ^==
; the keys must be edn, the values are code to be evaluated
#?(:clj
   (defmacro load-vedn-from-file [filename]
     (->> (macroexpand `(template/load-resource ~filename))
          (split-vedn-list)
          (drop 1)
          (partition 2)
          (mapv (fn [[k v]]
                  (let [code-str (string/trim v)
                        code (read-string code-str)]
                    ; cannot str-and-code' until runtime https://dev.clojure.org/jira/browse/CLJ-1206
                    [(read-string k) `(str-and-code' ~code ~code-str)])))
          (into {}))))
