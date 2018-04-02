(ns contrib.datomic-errors-test
  (:require [#?(:clj clojure.test :cljs cljs.test)
             #?(:clj :refer :cljs :refer-macros) [deftest is]]
            [contrib.datomic-errors :refer [parse-datomic-error-soup datomic-error-cleaner]]))


(def errors
  {"java.lang.IllegalArgumentException: :db.error/insufficient-binding Insufficient binding of db clause: [?e] would cause full scan"
   [:db.error/insufficient-binding "Insufficient binding of db clause: [?e] would cause full scan"]
   "java.lang.Exception: processing rule: (q__28535 ?e), message: processing clause: [?e :todo5/title], message: :db.error/not-an-entity Unable to resolve entity: :todo5/title"
   [:db.error/not-an-entity "Unable to resolve entity: :todo5/title"]
   })

(deftest parse-datomic-error-soup-1
  []
  (for [[k v] errors]
    (is (= v (parse-datomic-error-soup k)))))
