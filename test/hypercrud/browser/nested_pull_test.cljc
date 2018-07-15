(ns hypercrud.browser.nested-pull-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.walk :as walk]
            [contrib.data :as data]
            [contrib.reactive :as r]
            [hypercrud.browser.field :as field :refer [auto-fields]]
            [hypercrud.browser.system-link :refer [system-links]]
            [hypercrud.types.QueryRequest :refer [->QueryRequest]]))


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

(defn build-ctx [fiddle request result]
  {:hypercrud.browser/fiddle (r/atom fiddle)
   :hypercrud.browser/request (r/atom request)
   :hypercrud.browser/result (r/atom result)
   :hypercrud.browser/schemas (r/atom {"$" test-schema})})

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
                                {"$" nil "?e" 1})
        result nil
        ctx (build-ctx fiddle request result)
        fields @(auto-fields ctx)]

    (let [expected [{::field/data-has-id? true
                     ::field/path-segment 0
                     ::field/source-symbol '$
                     ::field/children [{::field/data-has-id? false
                                        ::field/path-segment :some-string}
                                       {::field/data-has-id? true
                                        ::field/path-segment :one-ref1}
                                       {::field/data-has-id? true
                                        ::field/path-segment :one-ref2
                                        ::field/children [{::field/data-has-id? false
                                                           ::field/path-segment :some-string}
                                                          {::field/data-has-id? true
                                                           ::field/path-segment :one-ref1
                                                           ::field/children [{::field/data-has-id? false
                                                                              ::field/path-segment :some-string}]}
                                                          {::field/data-has-id? true
                                                           ::field/path-segment :many-ref1
                                                           ::field/children [{::field/data-has-id? false
                                                                              ::field/path-segment :some-string}]}]}
                                       {::field/data-has-id? true
                                        ::field/path-segment :many-ref1}
                                       {::field/data-has-id? true
                                        ::field/path-segment :many-ref2
                                        ::field/children [{::field/data-has-id? false
                                                           ::field/path-segment :some-string}
                                                          {::field/data-has-id? true
                                                           ::field/path-segment :one-ref1
                                                           ::field/children [{::field/data-has-id? false
                                                                              ::field/path-segment :some-string}]}
                                                          {::field/data-has-id? true
                                                           ::field/path-segment :many-ref1
                                                           ::field/children [{::field/data-has-id? false
                                                                              ::field/path-segment :some-string}]}]}]}]
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
                   fields)]
      (is (= expected actual)))

    (let [links (try (system-links fiddle fields @(:hypercrud.browser/schemas ctx))
                     (catch Exception e
                       (println (pr-str e))
                       (throw e)))
          expected #{{:link/path ":head 0" :link/rel :hyperfiddle/new}
                     {:link/path ":body 0" :link/rel :hyperfiddle/edit}
                     {:link/path ":body 0" :link/rel :hyperfiddle/remove}

                     {:link/path ":body 0 :one-ref1" :link/rel :hyperfiddle/new}
                     {:link/path ":body 0 :one-ref1" :link/rel :hyperfiddle/edit}
                     {:link/path ":body 0 :one-ref1" :link/rel :hyperfiddle/remove}

                     {:link/path ":body 0 :one-ref2" :link/rel :hyperfiddle/new}
                     {:link/path ":body 0 :one-ref2" :link/rel :hyperfiddle/edit}
                     {:link/path ":body 0 :one-ref2" :link/rel :hyperfiddle/remove}

                     {:link/path ":body 0 :one-ref2 :body :one-ref1" :link/rel :hyperfiddle/new}
                     {:link/path ":body 0 :one-ref2 :body :one-ref1" :link/rel :hyperfiddle/edit}
                     {:link/path ":body 0 :one-ref2 :body :one-ref1" :link/rel :hyperfiddle/remove}

                     {:link/path ":body 0 :one-ref2 :body :many-ref1" :link/rel :hyperfiddle/new}
                     {:link/path ":body 0 :one-ref2 :body :many-ref1 :body" :link/rel :hyperfiddle/edit}
                     {:link/path ":body 0 :one-ref2 :body :many-ref1 :body" :link/rel :hyperfiddle/remove}

                     {:link/path ":body 0 :many-ref1" :link/rel :hyperfiddle/new}
                     {:link/path ":body 0 :many-ref1 :body" :link/rel :hyperfiddle/edit}
                     {:link/path ":body 0 :many-ref1 :body" :link/rel :hyperfiddle/remove}

                     {:link/path ":body 0 :many-ref2" :link/rel :hyperfiddle/new}
                     {:link/path ":body 0 :many-ref2 :body" :link/rel :hyperfiddle/edit}
                     {:link/path ":body 0 :many-ref2 :body" :link/rel :hyperfiddle/remove}

                     {:link/path ":body 0 :many-ref2 :body :one-ref1" :link/rel :hyperfiddle/new}
                     {:link/path ":body 0 :many-ref2 :body :one-ref1" :link/rel :hyperfiddle/edit}
                     {:link/path ":body 0 :many-ref2 :body :one-ref1" :link/rel :hyperfiddle/remove}

                     {:link/path ":body 0 :many-ref2 :body :many-ref1" :link/rel :hyperfiddle/new}
                     {:link/path ":body 0 :many-ref2 :body :many-ref1 :body" :link/rel :hyperfiddle/edit}
                     {:link/path ":body 0 :many-ref2 :body :many-ref1 :body" :link/rel :hyperfiddle/remove}}
          actual (->> links
                      (walk/prewalk (fn [x]
                                      (if (map? x)
                                        ; just testing the structural attributes
                                        (->> (select-keys x [:link/path
                                                             :link/rel
                                                             #_:link/disabled?])
                                             (remove (comp nil? second))
                                             (into {}))
                                        x)))
                      (into #{}))]
      (is (= expected actual)))))
