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
; the keys must be edn
#?(:clj
   (defmacro load-vedn-from-file [filename]
     (->> (macroexpand `(template/load-resource ~filename))
          (split-vedn-list)
          (drop 1)
          (partition 2)
          (map (fn [[k v]] [(read-string k) (string/trim v)]))
          (into {}))))
