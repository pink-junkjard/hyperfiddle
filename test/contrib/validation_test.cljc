(ns contrib.validation-test
  (:require
    [clojure.spec.alpha :as s]
    [clojure.test :refer [deftest is]]
    [contrib.data :refer [collect]]
    [contrib.datomic]
    [contrib.validation :refer [explained-for-view form-validation-hints]]))


(def r1 [{:foo/bar 1 :db/id 123}
         {:foo/baz 42 :db/id 124}])
(def e1 (s/explain-data (s/coll-of (s/keys :req [:foo/bar])) r1))

(deftest validation-1
  []
  (is (= (-> (explained-for-view contrib.datomic/smart-lookup-ref-no-tempids e1) ::s/problems form-validation-hints)
         '([[124 :foo/bar] :contrib.validation/missing])))
  )