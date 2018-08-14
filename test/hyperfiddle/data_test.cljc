(ns hyperfiddle.data-test
  (:require
    [clojure.test :refer [deftest is]]
    [hyperfiddle.data :as data]
    ))


(deftest deps-satisfied-1
  []
  (is (= (data/deps-satisfied? [:body 0 :fiddle/links] [:body 0 :fiddle/links :body :link/fiddle]) false))
  (is (= (data/deps-satisfied? [:body 0 :gender] [:body 0 :shirt-size]) true))
  (is (= (data/deps-satisfied? [:body 0] [:body 0 :shirt-size]) true))
  (is (= (data/deps-satisfied? [:body 0] [:body 0 :fiddle/links :body :link/fiddle]) false))
  (is (= (data/deps-satisfied? [:body 0 :fiddle/links] [:body 0 :fiddle/links :body :link/fiddle]) false))
  (is (= (data/deps-satisfied? [] [:body 0]) false))
  (is (= (data/deps-satisfied? [] [:body 0 :user/user-id]) false)) ; This becomes true if a relation can be implied in scope
  (is (= (data/deps-satisfied? [:body] [:body 0 :user/user-id]) true))
  (is (= (data/deps-satisfied? [:body 0 :user/user-id] []) true))
  )

(deftest link-path-floor-1
  []
  (is (= (data/link-path-floor [:body 0 :user/user-id]) [:body 0]))
  (is (= (data/link-path-floor [:body 0 :reg/gender]) [:body 0]))
  (is (= (data/link-path-floor [:body 0 :fiddle/links :body :link/fiddle]) [:body 0 :fiddle/links :body]))
  )

;(deftest deps-over-satisfied-1
;  []
;  (is (= (data/deps-over-satisfied? [:body 0 :fiddle/links] [:body 0 :fiddle/links :body :link/fiddle]) true))
;  (is (= (data/deps-over-satisfied? [:body 0 :gender] [:body 0 :shirt-size]) true))
;  (is (= (data/deps-over-satisfied? [:body 0] [:body 0 :shirt-size]) true))
;  (is (= (data/deps-over-satisfied? [:body 0] [:body 0 :fiddle/links :body :link/fiddle]) true))
;  (is (= (data/deps-over-satisfied? [:body 0 :fiddle/links] [:body 0 :fiddle/links :body :link/fiddle]) true))
;  (is (= (data/deps-over-satisfied? [] [:body 0]) true))
;  (is (= (data/deps-over-satisfied? [:body 0] []) false))
;  (is (= (data/deps-over-satisfied? [] [:body 0 :user/user-id]) true))
;  (is (= (data/deps-over-satisfied? [:body] [:body 0 :user/user-id]) true))
;  (is (= (data/deps-over-satisfied? [:body 0 :user/user-id] []) false))
;  )

(deftest newtest-1
  []
  ; !field(0) is oversatisfied for genders
  (is (= (data/deps-satisfied? [:body 0] []) true))         ; body count is >=
  (is (= (data/deps-over-satisfied? [:body 0] []) true))    ; left divergence has :body

  ; !field(0) is satisfied for shirt-sizes
  (is (= (data/deps-satisfied? [:body 0] [:body 0 :reg/gender]) true)) ; body count is =
  (is (= (data/deps-over-satisfied? [:body 0] [:body 0 :reg/gender]) false))
  (is (= (data/deps-over-satisfied? [:body 0 :reg/gender] [:body 0 :reg/gender]) true))
  ; It would be over-satisfied if there was another :body
  )


