(ns hyperfiddle.data-test
  (:require
    [clojure.test :refer [deftest is]]
    [hyperfiddle.data :as data]
    ))


(deftest link-path-
  []
  (is (= (data/deps-satisfied? [:body 0 :fiddle/links] [:body 0 :fiddle/links :body :link/fiddle]) false))
  (is (= (data/deps-satisfied? [:body 0 :gender] [:body 0 :shirt-size]) true))
  (is (= (data/deps-satisfied? [:body 0] [:body 0 :shirt-size]) true))
  (is (= (data/deps-satisfied? [:body 0] [:body 0 :fiddle/links :body :link/fiddle]) false))
  (is (= (data/deps-satisfied? [:body 0 :fiddle/links] [:body 0 :fiddle/links :body :link/fiddle]) false))
  (is (= (data/deps-satisfied? [] [:body 0]) false))
  (is (= (data/deps-satisfied? [:body 0] []) false))
  (is (= (data/deps-satisfied? [] [:body 0 :user/user-id]) false)) ; This becomes true if a relation can be implied in scope
  (is (= (data/deps-satisfied? [:body 0 :user/user-id] []) true))
  )
