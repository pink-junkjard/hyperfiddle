(ns hypercrud.browser.auto-link-formula-test
  (:require [clojure.test :refer [deftest is]]
            [contrib.eval :as eval]
            [contrib.reactive :as r]
            [hypercrud.browser.auto-link-formula :refer [auto-entity auto-entity-from-stage
                                                         auto-formula-lookup
                                                         deterministic-ident]]
            [hypercrud.types.Entity :refer [->Entity]]))


; semi-useful scaffolding
#_(deftest test-deterministic-ident
    (let [ctx {:hypercrud.browser/find-element (r/atom {:name "?e"})
               :cell-data (r/atom {:db/id "entity"})
               :hypercrud.browser/fat-attribute (r/atom {:db/ident :some/attr
                                                         :db/cardinality {:db/ident :db.cardinality/one}})
               :value nil}]
      (is (= "-2095329786" (deterministic-ident ctx)))))

(deftest fe-no-create []
  (let [f (eval/eval-string (get auto-formula-lookup {:fe true :c? false :d? true :a false}))
        e (->Entity #uri "test" {:db/id "entity"})
        ctx {:cell-data (r/atom e)}]
    (is (= (f ctx) e)))

  (let [f (eval/eval-string (get auto-formula-lookup {:fe true :c? false :d? true :a true}))
        e (->Entity #uri "test" {:db/id "entity"})
        ctx {:cell-data (r/atom e)
             :hypercrud.browser/attribute :some/attr
             :hypercrud.browser/fat-attribute (r/atom {:db/ident :some/attr
                                                       :db/cardinality {:db/ident :db.cardinality/one}})
             :value (r/atom 1)}]
    (is (= (f ctx) 1)))

  (let [f (eval/eval-string (get auto-formula-lookup {:fe true :c? false :d? true :a true}))
        e (->Entity #uri "test" {:db/id "entity"})
        ctx {:cell-data (r/atom e)
             :hypercrud.browser/attribute :some/attr-many
             :hypercrud.browser/fat-attribute (r/atom {:db/ident :some/attr-many
                                                       :db/cardinality {:db/ident :db.cardinality/many}})
             :value (r/atom 1)}]
    (is (= (f ctx) [e :some/attr-many])))

  (let [f (eval/eval-string (get auto-formula-lookup {:fe true :c? false :d? false :a false}))
        ctx {}]
    (is (nil? (f ctx))))

  (let [f (eval/eval-string (get auto-formula-lookup {:fe true :c? false :d? false :a true}))
        ctx {}]
    (is (nil? (f ctx)))))
