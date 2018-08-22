(ns hyperfiddle.data-test
  (:require
    [clojure.test :refer [deftest is]]
    [hyperfiddle.data :as data]
    ))


#_(deftest deps-satisfied-1
  []
  (is (= (data/deps-satisfied? [0 :fiddle/links] [0 :fiddle/links :link/fiddle]) false))
  (is (= (data/deps-satisfied? [0 :gender] [0 :shirt-size]) true))
  (is (= (data/deps-satisfied? [0] [0 :shirt-size]) true))
  (is (= (data/deps-satisfied? [0] [0 :fiddle/links :link/fiddle]) false))
  (is (= (data/deps-satisfied? [0 :fiddle/links] [0 :fiddle/links :link/fiddle]) false))
  (is (= (data/deps-satisfied? [] [0]) false))
  (is (= (data/deps-satisfied? [] [0 :user/user-id]) false)) ; This becomes true if a relation can be implied in scope
  (is (= (data/deps-satisfied? [] [0 :user/user-id]) true))
  (is (= (data/deps-satisfied? [0 :user/user-id] []) true))
  )

(deftest link-path-floor-1
  []
  (is (= (data/link-path-floor [0 :user/user-id]) [0]))
  (is (= (data/link-path-floor [0 :reg/gender]) [0]))
  (is (= (data/link-path-floor [0 :fiddle/links :link/fiddle]) [0 :fiddle/links]))
  )

;(deftest deps-over-satisfied-1
;  []
;  (is (= (data/deps-over-satisfied? [0 :fiddle/links] [0 :fiddle/links :link/fiddle]) true))
;  (is (= (data/deps-over-satisfied? [0 :gender] [0 :shirt-size]) true))
;  (is (= (data/deps-over-satisfied? [0] [0 :shirt-size]) true))
;  (is (= (data/deps-over-satisfied? [0] [0 :fiddle/links :link/fiddle]) true))
;  (is (= (data/deps-over-satisfied? [0 :fiddle/links] [0 :fiddle/links :link/fiddle]) true))
;  (is (= (data/deps-over-satisfied? [] [0]) true))
;  (is (= (data/deps-over-satisfied? [0] []) false))
;  (is (= (data/deps-over-satisfied? [] [0 :user/user-id]) true))
;  (is (= (data/deps-over-satisfied? [] [0 :user/user-id]) true))
;  (is (= (data/deps-over-satisfied? [0 :user/user-id] []) false))
;  )

(deftest newtest-1
  []
  ; !field(0) is oversatisfied for genders
  (is (= (data/deps-satisfied? [0] []) true))               ; body count is >=
  ;(is (= (data/deps-over-satisfied? [0] []) true))          ; left divergence has

  ; !field(0) is satisfied for shirt-sizes
  (is (= (data/deps-satisfied? [0] [0 :reg/gender]) true))  ; body count is =
  ;(is (= (data/deps-over-satisfied? [0] [0 :reg/gender]) false))
  ;(is (= (data/deps-over-satisfied? [0 :reg/gender] [0 :reg/gender]) true))
  ; It would be over-satisfied if there was another 
  )


