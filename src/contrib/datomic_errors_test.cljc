(ns contrib.datomic-errors-test
  (:require [clojure.test :refer [deftest is]]
            [contrib.datomic-errors :refer [parse-datomic-error-soup]]))


(def errors
  {"java.lang.IllegalArgumentException: :db.error/insufficient-binding Insufficient binding of db clause: [?e] would cause full scan"
   [:db.error/insufficient-binding "Insufficient binding of db clause: [?e] would cause full scan"]
   "java.lang.Exception: processing rule: (q__28535 ?e), message: processing clause: [?e :todo5/title], message: :db.error/not-an-entity Unable to resolve entity: :todo5/title"
   [:db.error/not-an-entity "Unable to resolve entity: :todo5/title"]

   ; newlines in this msg
   #_#_"java.lang.IllegalArgumentException: :db.error/datoms-conflict Two datoms in the same transaction conflict
{:d1 [17592186045931 :fiddle/type :entity 13194139534826 true],
 :d2 [17592186045931 :fiddle/type :query 13194139534826 true]}
"
   [:db.error/datoms-conflict
    "Two datoms in the same transaction conflict\n{:d1 [17592186045931 :fiddle/type :entity 13194139534826 true],\n :d2 [17592186045931 :fiddle/type :query 13194139534826 true]}\n"]

   })

(deftest parse-datomic-error-soup-1
  []
  (for [[k v] errors]
    (is (= v (parse-datomic-error-soup k nil)))))
