(ns contrib.string
  (:require [cats.monad.either :as either]
            [clojure.string]
            [contrib.data :refer [orp]]
            [contrib.reader :refer [read-edn-string]]
            [contrib.try$ :refer [try-either]]
            [cuerdas.core :as str]))


;(defn split-last [s sep]
;  (let [[x & xs] (str/split (reverse s) sep)]
;    [x (reverse (str/join xs))]))
;
;(comment
;  (split-last "asdf#frag" "#")
;  (split-last "asdf#frag" "#"))

(defn empty->nil [s]
  (if (str/empty-or-nil? s) nil s))

(defn blank->nil [s]
  (if (and (string? s)
           (not (str/blank? s)))
    s))

(comment
  (merge-with #(or (blank->nil %1) %2)
              {:a "a" :b nil :d ""}
              {:a "aaa" :b "bbb" :c "ccc" :d "ddd"})
  (merge-with #(orp (complement blank->nil) %1 %2)
              {:a "a" :b nil :d ""}
              {:a "aaa" :b "bbb" :c "ccc" :d "ddd"})

  (orp (complement blank->nil) "" "aaa")
  )

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

(defn or-str [& args]                                       ; todo macro
  (apply orp str/empty-or-nil? args))
