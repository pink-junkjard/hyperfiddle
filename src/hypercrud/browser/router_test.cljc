(ns hypercrud.browser.router-test
  (:require [clojure.test :refer [deftest is]]
            [contrib.reader]
            [hypercrud.browser.router :refer [encode decode canonicalize]]
            [hypercrud.browser.routing :refer [normalize-args]]))


(def route-args2 [:hyperfiddle.blog/post [#entity["$" [:user/sub "google-oauth2|116635422485042503270"]] #{"events" "news"}]])
(def route-args1 [:hyperfiddle.blog/post #entity["$" [:user/sub "google-oauth2|116635422485042503270"]]])
(def route-args1-seq [:hyperfiddle.blog/post [#entity["$" [:user/sub "google-oauth2|116635422485042503270"]]]])

(deftest router-basic
  []
  (is (= (encode route-args2) "/:hyperfiddle.blog!post/~entity('$',(:user!sub,'google-oauth2%7C116635422485042503270'))/~%7B'news','events'%7D"))
  (is (= (decode "/17592186045933/") [17592186045933]))     ; legacy, doesnt happen anymore?
  (is (= ((comp decode encode) route-args2) route-args2))
  #_(is (= ((comp decode encode) route-args1) (update route-args1 1 normalize-args)))
  (is (= ((comp decode encode) route-args1-seq) route-args1-seq))
  #_(is (= ((comp decode encode) route-args1) ((comp decode encode) route-args1-seq)))
  #?(:clj (is (not (nil? (java.net.URI. (encode route-args2))))))
  (is (= (encode [:hyperfiddle.blog/post])
         (encode [:hyperfiddle.blog/post])
         (encode [:hyperfiddle.blog/post])
         (encode [:hyperfiddle.blog/post '()])
         "/:hyperfiddle.blog!post/"))
  (is (= (encode [17592186045502])
         (encode [17592186045502 nil])
         (encode [17592186045502 []])
         (encode [17592186045502 '()])
         "/17592186045502/"))
  (is (= (encode [:hyperfiddle.blog/post []]) "/:hyperfiddle.blog!post/"))

  (is (= (encode [:hyperblog/post [#entity["$" 17592186045826]]])
         "/:hyperblog!post/~entity('$',17592186045826)"))

  (is (= (decode "/:hyperfiddle.blog!post/~entity('$',:hyperfiddle.blog!homepage)") [:hyperfiddle.blog/post [#entity["$" :hyperfiddle.blog/homepage]]]))
  (is (= (decode "/:hyperfiddle.blog!post/~entity('$',:hyperfiddle.blog!homepage)?#:src") [:hyperfiddle.blog/post [#entity["$" :hyperfiddle.blog/homepage]] nil ":src"]))
  (is (= (decode "/:hyperfiddle.blog!post/~entity('$',:hyperfiddle.blog!homepage)#:src") [:hyperfiddle.blog/post [#entity["$" :hyperfiddle.blog/homepage]] nil ":src"]))

  (is (= "/:hyperfiddle.blog!post/~entity('$',:hyperfiddle.blog!homepage)" (encode [:hyperfiddle.blog/post [#entity["$" :hyperfiddle.blog/homepage]]])))
  ;(is (= "/:hyperfiddle.blog!post/~entity('$',:hyperfiddle.blog!homepage)?#:src" (encode [:hyperfiddle.blog/post [#entity["$" :hyperfiddle.blog/homepage]] nil ":src"])))

  )

(deftest router-malformed-1
  []
  ;#entity["$" [:user/sub "google-oauth2|116635422485042503270"]]
  ;(decode "/") [nil [nil]]
  ;(decode "/garbagasdf..23425649=//e")
  ;(decode "/asdf/asdf/asdf?asdf?asdf?sadf")
  )

(deftest canonicalize-1 []
  (is (= (apply canonicalize [nil nil nil nil]) nil))
  (is (= (apply canonicalize []) nil))
  )

(deftest fragment-1 []
  (is (= "/:hyperfiddle.blog!post/~entity('$',:hyperfiddle.blog!homepage)#:src" (encode [:hyperfiddle.blog/post [#entity["$" :hyperfiddle.blog/homepage]] nil ":src"])))
  (is (= "/:hyperblog!post/~entity('$',:capitalism)#:src" (encode [:hyperblog/post [#entity["$" :capitalism]] nil ":src"]))))
