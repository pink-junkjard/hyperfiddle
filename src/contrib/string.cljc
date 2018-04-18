(ns contrib.string
  (:refer-clojure :exclude [read-string])
  #?(:cljs (:require-macros [contrib.data]))
  (:require [cats.monad.either :as either]
            [contrib.data :refer [orp]]
            [contrib.try :refer [try-either]]
            [contrib.reader :refer [read-edn-string]]
            [cuerdas.core :as str]
            [clojure.string]
            [net.cgrand.packed-printer :as packed-printer]
    #?(:clj
            [clojure.pprint :as pprint]
       :cljs [cljs.pprint :as pprint])))


;(defn split-last [s sep]
;  (let [[x & xs] (str/split (reverse s) sep)]
;    [x (reverse (str/join xs))]))
;
;(comment
;  (split-last "asdf#frag" "#")
;  (split-last "asdf#frag" "#"))

(defn empty->nil [s]
  (if (str/empty-or-nil? s) nil s))

(defn split-first [s sep]
  (let [[x & xs] (str/split s sep)]
    [(empty->nil x) (empty->nil (str/join sep xs))]))

(defn abc []
  (map (comp keyword str) "abcdefghijklmnopqrstuvwxyz")     ; this version works in clojurescript
  #_(->> (range) (map (comp keyword str char #(+ % (int \a))))))

(defn safe-read-edn-string [user-edn-str]                   ; is this private? Should this ever be called? Isn't it slow?
  (if user-edn-str
    ; this doesn't handle sharp-lambdas
    (try-either (read-edn-string user-edn-str))
    (either/right nil)))

(def memoized-safe-read-edn-string (memoize safe-read-edn-string))

(defn slow-pprint-str [v & [columns]]
  (with-out-str
    (packed-printer/pprint v :width (or columns pprint/*print-right-margin*))))

(defn pprint-str [v & [columns]]
  (clojure.string/trimr
    ; Previously, pprint/*print-miser-width* was set to nil in main
    (binding [pprint/*print-right-margin* (or columns pprint/*print-right-margin*)]
      (with-out-str
        (pprint/pprint v)))))

#?(:clj (defmacro mpprint-str [& args] (apply slow-pprint-str args)))

(defn or-str [& args]                                       ; todo macro
  (apply orp str/empty-or-nil? args))
