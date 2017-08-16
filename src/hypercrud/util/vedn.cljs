(ns hypercrud.util.vedn
  (:require [cljs.reader :as reader]
            [clojure.string :as string]
            [hypercrud.compile.eval :as eval]
            [hypercrud.compile.macros :refer [str-and-code']]))


(def vedn-delimiter "(?m)^==")

(defn split-vedn-list [data-str]
  (string/split data-str (re-pattern vedn-delimiter)))

; a format, a map written in list form, delimited by ^==
; the keys must be edn, the values are code to be evaluated
(defn read-string [data-str]
  (->> (split-vedn-list data-str)
       (drop 1)
       (partition 2)
       (mapv (fn [[k v]]
               (let [code-str (string/trim v)
                     code (eval/eval-str-and-throw code-str)]
                   [(reader/read-string k) (str-and-code' code code-str)])))
       (into {})))
