(ns contrib.match-test
  (:require
    [clojure.core.match :refer [match #?(:cljs match*)]]
    [clojure.test :refer [deftest is]]))


(deftest match-class-
  []
  (is (= #?(:clj  (let [String String
                        Long Long]
                    (match
                      [(type "1") (type 1)]
                      [String Long] :pass
                      [_ _] :fail))

            :cljs :pass
            #_(let [String js/String
                    Number js/Number]
                (match
                  [(type "1") (type 1)]
                  [String Number] :pass
                  [_ _] :fail)))
         :pass))
  )
