(ns hypercrud.browser.router-test
  (:require [#?(:clj clojure.test :cljs cljs.test)
             #?(:clj :refer :cljs :refer-macros) [deftest is]]
            [hypercrud.compile.reader]
            [hypercrud.browser.router :refer [encode decode]]
            ))



(def route {:fiddle-id :hyperfiddle.blog/post
            :domain-ident "kobe"
            :request-params [#entity["$" [:user/sub "google-oauth2|116635422485042503270"]]
                             #{"events" "news"}]})

(def route-str "/:hyperfiddle.blog!post;'kobe'/~entity('$',(:user!sub,'google-oauth2%7c116635422485042503270'))/~%7b'news','events'%7d")

(deftest router-1
  []
  (is (= (encode route) route-str))
  (is (= ((comp decode encode) (dissoc route :domain-ident)) (dissoc route :domain-ident)))

  #?(:clj
     (is (not (nil? (java.net.URI. route-str)))))

  ; malformed?
  ;(decode "/") {:fiddle-id nil, :request-params [nil]}
  ;(decode "/garbagasdf..23425649=//e") {:fiddle-id garbagasdf..23425649=, :request-params [nil e]}
  ;(decode "/asdf/asdf/asdf?asdf?asdf?sadf") {:fiddle-id asdf, :request-params [asdf asdf]}
  )
