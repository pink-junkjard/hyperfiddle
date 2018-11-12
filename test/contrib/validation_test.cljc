(ns contrib.validation-test
  (:require
    [clojure.spec.alpha :as s]
    [clojure.test :refer [deftest is]]
    [contrib.data :refer [collect]]
    [contrib.datomic]
    [contrib.validation :refer [validate explained-for-view form-validation-hints]]))


(deftest validation-1
  []
  (is (= (validate (s/coll-of (s/keys :req [:foo/bar]))
                   [{:foo/bar 1 :db/id 123}
                    {:foo/baz 42 :db/id 124}]
                   contrib.datomic/smart-lookup-ref-no-tempids)
         '([[124 :foo/bar] :contrib.validation/missing])))
  )
