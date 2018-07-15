(ns hypercrud.browser.field-nested-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.walk :as walk]
            [contrib.data :as data]
            [contrib.reactive :as r]
            [hypercrud.browser.field :as field :refer [auto-fields]]
            [hypercrud.types.QueryRequest :refer [->QueryRequest]]))


(def test-dbname "test")
(def test-schema (->> [{:db/ident :some-string
                        :db/cardinality {:db/ident :db.cardinality/one}
                        :db/valueType {:db/ident :db.type/string}}
                       {:db/ident :one-ref1
                        :db/cardinality {:db/ident :db.cardinality/one}
                        :db/valueType {:db/ident :db.type/ref}}
                       {:db/ident :one-ref2
                        :db/cardinality {:db/ident :db.cardinality/one}
                        :db/valueType {:db/ident :db.type/ref}}
                       {:db/ident :many-ref1
                        :db/cardinality {:db/ident :db.cardinality/many}
                        :db/valueType {:db/ident :db.type/ref}}
                       {:db/ident :many-ref2
                        :db/cardinality {:db/ident :db.cardinality/many}
                        :db/valueType {:db/ident :db.type/ref}}]
                      (data/group-by-assume-unique :db/ident)))

(defn build-ctx [fiddle request result]
  {:hypercrud.browser/fiddle (r/atom fiddle)
   :hypercrud.browser/request (r/atom request)
   :hypercrud.browser/result (r/atom result)
   :hypercrud.browser/schemas (r/atom {test-dbname test-schema})})

(deftest nested-tests []
  (let [fiddle {:fiddle/type :query
                :fiddle/pull-database test-dbname}
        request (->QueryRequest '[:find (pull ?e [:db/id
                                                  :some-string
                                                  :one-ref1
                                                  {:one-ref2 [:db/id :some-string]}
                                                  :many-ref1
                                                  {:many-ref2 [:db/id :some-string]}])
                                  :in $ ?e]
                                {"$" nil "?e" 1})
        result nil
        ctx (build-ctx fiddle request result)
        expected [{::field/data-has-id? true
                   ::field/path-segment 0
                   ::field/source-symbol '$
                   ::field/children [{::field/data-has-id? false
                                      ::field/path-segment :some-string}
                                     {::field/data-has-id? false
                                      ::field/path-segment :one-ref1}
                                     {::field/data-has-id? true
                                      ::field/path-segment :one-ref2
                                      ::field/children [{::field/data-has-id? false
                                                         ::field/path-segment :some-string}]}
                                     {::field/data-has-id? false
                                      ::field/path-segment :many-ref1}
                                     {::field/data-has-id? true
                                      ::field/path-segment :many-ref2
                                      ::field/children [{::field/data-has-id? false
                                                         ::field/path-segment :some-string}]}]}]
        fields (walk/prewalk
                 (fn [x]
                   (if (map? x)
                     ; just testing the structural attributes
                     (->> (select-keys x [::field/data-has-id?
                                          ::field/path-segment
                                          ::field/source-symbol
                                          ::field/children])
                          (remove (comp nil? second))
                          (into {}))
                     x))
                 @(auto-fields ctx))]
    (is (= expected fields fields))))
