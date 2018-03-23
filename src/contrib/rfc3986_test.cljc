(ns contrib.rfc3986-test
  (:require [#?(:clj clojure.test :cljs cljs.test)
             #?(:clj :refer :cljs :refer-macros) [deftest is]]
            [contrib.rfc3986 :refer [encode-rfc3986-pchar decode-rfc3986-pchar
                                     encode-ednish decode-ednish]]
            ))

(deftest url-encode-1
  []
  (is (= (encode-rfc3986-pchar "google-oauth2|116635422485042503270")
         "google-oauth2%7c116635422485042503270")))

(deftest url-decode-1
  []
  (is (= (decode-rfc3986-pchar "google-oauth2%7c116635422485042503270")
         "google-oauth2|116635422485042503270")))

(deftest ednish-1
  []
  (is (= (encode-ednish (pr-str :hyperfiddle.blog/post))
         ":hyperfiddle.blog!post" ))
  (is (= (encode-ednish (pr-str "kobe"))
         "'kobe'"))
  (is (= (encode-ednish (pr-str #entity["$" [:user/sub "google-oauth2|116635422485042503270"]]))
         "~entity('$',(:user!sub,'google-oauth2|116635422485042503270'))"))
  (is (= (encode-ednish (pr-str #{"events" "news"}))
         "~{'news','events'}")))
