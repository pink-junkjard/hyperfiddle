(ns hypercrud.browser.nested-pull-test
  (:require
    [cats.monad.exception :as exception]
    [clojure.test :refer [deftest is testing]]
    [clojure.walk :as walk]
    [contrib.data :as data]
    [contrib.reactive :as r]
    [hypercrud.browser.field :as field :refer [auto-field]]
    [hypercrud.types.DbRef :refer [->DbRef]]
    [hypercrud.types.QueryRequest :refer [->QueryRequest]]
    [hyperfiddle.foundation :as foundation]
    [hyperfiddle.runtime :as runtime]))


(def test-schema
  (->> [{:db/ident :some-string
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

(defn build-ctx [fiddle result]                             ; this is starting to look a lot like base/process-results
  {:branch foundation/root-branch
   :hypercrud.browser/fiddle (r/atom fiddle)
   :hypercrud.browser/data (r/atom result)
   :peer (let [state (r/atom {::runtime/partitions {foundation/root-branch {:schemas {"$" (exception/success test-schema)}}}})]
           (reify
             runtime/State
             (runtime/state [_] state)
             (runtime/state [_ path] (r/cursor state path))))})

(deftest nested-tests []
  (let [fiddle {:fiddle/type :query
                :fiddle/pull-database "$"}
        request (->QueryRequest '[:find (pull ?e [:db/id
                                                  :some-string
                                                  :one-ref1
                                                  {:one-ref2 [:db/id
                                                              :some-string
                                                              {:one-ref1 [:db/id :some-string]
                                                               :many-ref1 [:db/id :some-string]}]}
                                                  :many-ref1
                                                  {:many-ref2 [:db/id
                                                               :some-string
                                                               {:one-ref1 [:db/id :some-string]
                                                                :many-ref1 [:db/id :some-string]}]}])
                                  :in $ ?e]
                                [(->DbRef "$" nil) 1])
        result nil
        ctx (build-ctx fiddle result)
        field @(auto-field (r/atom request) ctx)]

    (let [expected [{:hypercrud.browser.field/data-has-id? true,
                     :hypercrud.browser.field/path-segment 0,
                     :hypercrud.browser.field/source-symbol '$,
                     :hypercrud.browser.field/children
                     [{:hypercrud.browser.field/data-has-id? false,
                       :hypercrud.browser.field/path-segment :db/id,
                       :hypercrud.browser.field/source-symbol '$}
                      {:hypercrud.browser.field/data-has-id? false,
                       :hypercrud.browser.field/path-segment :some-string,
                       :hypercrud.browser.field/source-symbol '$}
                      {:hypercrud.browser.field/data-has-id? true,
                       :hypercrud.browser.field/path-segment :one-ref1,
                       :hypercrud.browser.field/source-symbol '$}
                      {:hypercrud.browser.field/data-has-id? true,
                       :hypercrud.browser.field/path-segment :one-ref2,
                       :hypercrud.browser.field/source-symbol '$,
                       :hypercrud.browser.field/children
                       [{:hypercrud.browser.field/data-has-id? false,
                         :hypercrud.browser.field/path-segment :db/id,
                         :hypercrud.browser.field/source-symbol '$}
                        {:hypercrud.browser.field/data-has-id? false,
                         :hypercrud.browser.field/path-segment :some-string,
                         :hypercrud.browser.field/source-symbol '$}
                        {:hypercrud.browser.field/data-has-id? true,
                         :hypercrud.browser.field/path-segment :one-ref1,
                         :hypercrud.browser.field/source-symbol '$,
                         :hypercrud.browser.field/children
                         [{:hypercrud.browser.field/data-has-id? false,
                           :hypercrud.browser.field/path-segment :db/id,
                           :hypercrud.browser.field/source-symbol '$}
                          {:hypercrud.browser.field/data-has-id? false,
                           :hypercrud.browser.field/path-segment :some-string,
                           :hypercrud.browser.field/source-symbol '$}]}
                        {:hypercrud.browser.field/data-has-id? true,
                         :hypercrud.browser.field/path-segment :many-ref1,
                         :hypercrud.browser.field/source-symbol '$,
                         :hypercrud.browser.field/children
                         [{:hypercrud.browser.field/data-has-id? false,
                           :hypercrud.browser.field/path-segment :db/id,
                           :hypercrud.browser.field/source-symbol '$}
                          {:hypercrud.browser.field/data-has-id? false,
                           :hypercrud.browser.field/path-segment :some-string,
                           :hypercrud.browser.field/source-symbol '$}]}]}
                      {:hypercrud.browser.field/data-has-id? true,
                       :hypercrud.browser.field/path-segment :many-ref1,
                       :hypercrud.browser.field/source-symbol '$}
                      {:hypercrud.browser.field/data-has-id? true,
                       :hypercrud.browser.field/path-segment :many-ref2,
                       :hypercrud.browser.field/source-symbol '$,
                       :hypercrud.browser.field/children
                       [{:hypercrud.browser.field/data-has-id? false,
                         :hypercrud.browser.field/path-segment :db/id,
                         :hypercrud.browser.field/source-symbol '$}
                        {:hypercrud.browser.field/data-has-id? false,
                         :hypercrud.browser.field/path-segment :some-string,
                         :hypercrud.browser.field/source-symbol '$}
                        {:hypercrud.browser.field/data-has-id? true,
                         :hypercrud.browser.field/path-segment :one-ref1,
                         :hypercrud.browser.field/source-symbol '$,
                         :hypercrud.browser.field/children
                         [{:hypercrud.browser.field/data-has-id? false,
                           :hypercrud.browser.field/path-segment :db/id,
                           :hypercrud.browser.field/source-symbol '$}
                          {:hypercrud.browser.field/data-has-id? false,
                           :hypercrud.browser.field/path-segment :some-string,
                           :hypercrud.browser.field/source-symbol '$}]}
                        {:hypercrud.browser.field/data-has-id? true,
                         :hypercrud.browser.field/path-segment :many-ref1,
                         :hypercrud.browser.field/source-symbol '$,
                         :hypercrud.browser.field/children
                         [{:hypercrud.browser.field/data-has-id? false,
                           :hypercrud.browser.field/path-segment :db/id,
                           :hypercrud.browser.field/source-symbol '$}
                          {:hypercrud.browser.field/data-has-id? false,
                           :hypercrud.browser.field/path-segment :some-string,
                           :hypercrud.browser.field/source-symbol '$}]}]}]}]
          actual (walk/prewalk
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
                   (::field/children field))]
      (is (= expected actual)))

    ))
