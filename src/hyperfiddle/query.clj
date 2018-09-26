(ns hyperfiddle.query
  (:require
    [datomic.api :as d]))


; These are used from datalog e.g. [(hyperfiddle.query/attr-archived? ?attr)]

(defn attr-archived? [$ e-attr]
  (let [{a :db/ident} (d/pull $ [:db/ident] e-attr)]
    ; "zzz." and "zzz/"
    (some-> (namespace a) (clojure.string/starts-with? "zzz"))))

; https://forum.datomic.com/t/is-this-a-bug-in-datalog-symbol-resolution-with-not/613
(def attr-not-archived? (complement attr-archived?))

(defn attr-datomic? [$ e-attr]
  (<= e-attr 62))