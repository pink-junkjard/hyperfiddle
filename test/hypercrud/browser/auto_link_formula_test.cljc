(ns hypercrud.browser.auto-link-formula-test
  (:require [clojure.test :refer [deftest is]]
            [contrib.eval :as eval]
            [contrib.reactive :as r]
            [hypercrud.browser.auto-link-formula :refer [-auto-formula-impl deterministic-ident]]
            [hypercrud.types.Entity :refer [->Entity]]))


; semi-useful scaffolding
#_(deftest test-deterministic-ident
    (let [ctx {:hypercrud.browser/find-element (r/atom {:name "?e"})
               :cell-data (r/atom {:db/id "entity"})
               :hypercrud.browser/fat-attribute (r/atom {:db/ident :some/attr
                                                         :db/cardinality {:db/ident :db.cardinality/one}})
               :value nil}]
      (is (= "-2095329786" (deterministic-ident ctx)))))

(defn eval-formula [?s]
  (or (some-> ?s eval/eval-string)
      (constantly nil)))

(deftest fe-no-create []
  (let [$-schema {:some/attr {:db/ident :some/attr
                              :db/cardinality {:db/ident :db.cardinality/one}}
                  :some/attr-many {:db/ident :some/attr-many
                                   :db/cardinality {:db/ident :db.cardinality/many}}}
        base-ctx {:hypercrud.browser/ordered-fes (r/atom [{:source-symbol '$}])
                  :hypercrud.browser/schemas (r/atom {"$" $-schema})}]

    (let [f (eval-formula (-auto-formula-impl base-ctx [0] :create? false :dependent? true))
          e (->Entity #uri "test" {:db/id "entity"})
          ctx (assoc base-ctx :cell-data (r/atom e))]
      (is (= (f ctx) e)))

    (let [f (eval-formula (-auto-formula-impl base-ctx [0 :some/attr] :create? false :dependent? true))
          ctx (assoc base-ctx :value (r/atom 1))]
      (is (= (f ctx) 1)))

    (let [f (eval-formula (-auto-formula-impl base-ctx [0 :some/attr-many] :create? false :dependent? true))
          e (->Entity #uri "test" {:db/id "entity"})
          ctx (assoc base-ctx
                :cell-data (r/atom e)
                :hypercrud.browser/attribute :some/attr-many)]
      (is (= (f ctx) [e :some/attr-many])))

    (let [f (eval-formula (-auto-formula-impl base-ctx [0 :some/non-real-attr] :create? false :dependent? true))
          ctx (assoc base-ctx :value (r/atom 1))]
      (is (= (f ctx) 1)))

    (let [f (eval-formula (-auto-formula-impl base-ctx [0] :create? false :dependent? false))
          ctx base-ctx]
      (is (nil? (f ctx))))

    (let [f (eval-formula (-auto-formula-impl base-ctx [0 :some/attr] :create? false :dependent? false))
          ctx base-ctx]
      (is (nil? (f ctx))))))
