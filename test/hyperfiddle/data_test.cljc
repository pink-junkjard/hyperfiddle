(ns hyperfiddle.data-test
  (:require
    [clojure.test :refer [deftest is]]
    [hyperfiddle.data :as data]
    ))


(deftest link-path-
  []
  (is (= (data/matches-path? [:body 0 :fiddle/links] [:body 0 :fiddle/links :body :link/fiddle]) false))
  (is (= (data/matches-path? [:body 0 :gender] [:body 0 :shirt-size]) true))
  (is (= (data/matches-path? [:body 0] [:body 0 :shirt-size]) true))
  (is (= (data/matches-path? [:body 0] [:body 0 :fiddle/links :body :link/fiddle]) false))
  (is (= (data/matches-path? [:body 0 :fiddle/links] [:body 0 :fiddle/links :body :link/fiddle]) false))
  )
