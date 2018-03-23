(ns hypercrud.browser.router-test
  (:require [#?(:clj clojure.test :cljs cljs.test)
             #?(:clj :refer :cljs :refer-macros) [deftest is]]
            [hypercrud.compile.reader]
            [hypercrud.browser.router :refer [encode decode]]
            ))



(def kobe {:fiddle-id :hyperfiddle.blog/post
           :domain-ident "kobe"
           :request-params [#entity["$" [:user/sub "google-oauth2|116635422485042503270"]]
                            #{"events" "news"}]})

(def route-args1 {:fiddle-id :hyperfiddle.blog/post :request-params #entity["$" [:user/sub "google-oauth2|116635422485042503270"]]})
(def route-args1-seq {:fiddle-id :hyperfiddle.blog/post :request-params [#entity["$" [:user/sub "google-oauth2|116635422485042503270"]]]})

(deftest router-basic
  []
  (is (= (encode kobe) "/:hyperfiddle.blog!post;'kobe'/~entity('$',(:user!sub,'google-oauth2%7C116635422485042503270'))/~%7B'news','events'%7D"))
  (is (= (decode "/17592186045933/") {:fiddle-id 17592186045933}))
  (is (= ((comp decode encode) (dissoc kobe :domain-ident)) (dissoc kobe :domain-ident)))
  ;(is (= ((comp decode encode) kobe) kobe)) ; not implemented
  #_(is (= ((comp decode encode) route-args1) route-args1))
  (is (= ((comp decode encode) route-args1-seq) route-args1-seq))
  #_(is (= ((comp decode encode) route-args1) ((comp decode encode) route-args1-seq)))
  #?(:clj (is (not (nil? (java.net.URI. (encode kobe))))))
  (is (= (encode {:fiddle-id :hyperfiddle.blog/post})
         (encode {:fiddle-id :hyperfiddle.blog/post :request-params nil})
         (encode {:fiddle-id :hyperfiddle.blog/post :request-params []})
         (encode {:fiddle-id :hyperfiddle.blog/post :request-params '()})
         "/:hyperfiddle.blog!post/"))
  (is (= (encode {:fiddle-id 17592186045502})
         (encode {:fiddle-id 17592186045502 :request-params nil})
         (encode {:fiddle-id 17592186045502 :request-params []})
         (encode {:fiddle-id 17592186045502 :request-params '()})
         "/17592186045502/"))
  #_(is (= (encode {:fiddle-id :hyperfiddle.blog/post :request-params []}) "/:hyperfiddle.blog!post/"))
  )

(deftest router-malformed-1
  []
  ;#entity["$" [:user/sub "google-oauth2|116635422485042503270"]]
  ;(decode "/") {:fiddle-id nil, :request-params [nil]}
  ;(decode "/garbagasdf..23425649=//e") {:fiddle-id garbagasdf..23425649=, :request-params [nil e]}
  ;(decode "/asdf/asdf/asdf?asdf?asdf?sadf") {:fiddle-id asdf, :request-params [asdf asdf]}
  )


; /(:fiddle!ident,:litepay!superuser)/nil
