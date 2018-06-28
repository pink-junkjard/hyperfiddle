(ns contrib.ednish-test
  (:require
    [clojure.test :refer [deftest is]]
    [contrib.ednish :refer [encode-ednish decode-ednish]]))


(deftest ednish-1
  []
  (is (= (encode-ednish (pr-str :hyperfiddle.blog/post))
         ":hyperfiddle.blog!post"))
  (is (= (encode-ednish (pr-str "kobe"))
         "'kobe'"))
  (is (= (encode-ednish (pr-str #entity["$" [:user/sub "google-oauth2|116635422485042503270"]]))
         "~entity('$',(:user!sub,'google-oauth2|116635422485042503270'))"))
  (is (= (encode-ednish (pr-str #{"events" "news"}))
         "~{'news','events'}")))

(deftest
  ednish-tunneling
  []
  (def v #uri "datomic:free://datomic:4334/~dustin.getz")
  (def encoded (encode-ednish (pr-str v)))
  (pr-str v)
  (is (= (encode-ednish (pr-str v)) "~uri,'datomic:free:!!datomic:4334!~dustin.getz'"))
  ;(is (= (decode-ednish encoded) (pr-str v)))
  ; "#uri \"datomic:free://datomic:4334/#dustin.getz\""

  )