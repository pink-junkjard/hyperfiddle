(ns hyperfiddle.query
  (:require
    [contrib.ct :refer [unwrap]]
    [contrib.try$ :refer [try-either]]
    [datomic.api :as d]
    [hypercrud.browser.q-util :as q-util]
    [hypercrud.browser.base :as base]
    [hyperfiddle.fiddle :as fiddle]))


; These are used from datalog e.g. [(hyperfiddle.query/attr-archived? ?attr)]

(defn attr-archived? [$ e-attr]
  (let [{a :db/ident} (d/pull $ [:db/ident] e-attr)]
    ; "zzz." and "zzz/"
    (some-> (namespace a) (clojure.string/starts-with? "zzz"))))

; https://forum.datomic.com/t/is-this-a-bug-in-datalog-symbol-resolution-with-not/613
(def attr-not-archived? (complement attr-archived?))

(defn attr-datomic? [$ e-attr]
  (<= e-attr 62))

(defn entrypoint-fiddle? [$ e-fiddle]
  (let [fiddle (fiddle/fiddle-defaults (d/pull $ base/meta-pull-exp-for-link e-fiddle) nil)]
    (condp = (:fiddle/type fiddle)
      :blank true
      :entity false
      :query (empty? (q-util/args (:fiddle/query fiddle))))))
