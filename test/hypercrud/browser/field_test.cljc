(ns hypercrud.browser.field-test
  #?(:cljs (:require-macros [hypercrud.browser.field-test :refer [pull->attr-tests test-defined-pull test-partial-splat test-splat]]))
  (:require [clojure.test :refer [deftest is testing]]
            [contrib.reactive :as r]
            [hypercrud.browser.field :as field :refer [auto-fields infer-attrs]]
            [hypercrud.types.DbVal :refer [->DbVal]]
            [hypercrud.types.Entity :refer [->Entity]]
            [hypercrud.types.EntityRequest :refer [->EntityRequest]]
            [hypercrud.types.QueryRequest :refer [->QueryRequest]]
            [hypercrud.types.ThinEntity :refer [->ThinEntity]]))


(deftest test-infer-attr []
  (let [data (->Entity nil {:a/x 1
                            :a/y [(->Entity nil {:b/x 2
                                                 :b/y [{:asdf 1}
                                                       {:qwerty 3}]
                                                 :b/z {:c/a 1}})
                                  (->Entity nil {:b/x 3
                                                 :b/y [{:asdf2 1}]
                                                 :b/z {:c/b 2}})]
                            :a/z {:c/x 5}})]
    (is (= (infer-attrs data []) #{:a/x :a/y :a/z}))
    (is (= (infer-attrs data [:a/y]) #{:b/x :b/y :b/z}))
    (is (= (infer-attrs data [:a/y :b/y]) #{:asdf :qwerty :asdf2}))
    (is (= (infer-attrs data [:a/y :b/z]) #{:c/a :c/b}))
    (is (= (infer-attrs data [:a/z]) #{:c/x}))))

(def test-dbname "test")
(def test-schema {:a/j {:db/ident :a/j
                        :db/cardinality {:db/ident :db.cardinality/one}
                        :db/valueType {:db/ident :db.type/string}}
                  :a/k {:db/ident :a/k
                        :db/cardinality {:db/ident :db.cardinality/one}
                        :db/valueType {:db/ident :db.type/string}}
                  :a/s {:db/ident :a/s
                        :db/cardinality {:db/ident :db.cardinality/one}
                        :db/valueType {:db/ident :db.type/ref}}
                  :a/t {:db/ident :a/t
                        :db/cardinality {:db/ident :db.cardinality/one}
                        :db/valueType {:db/ident :db.type/ref}}
                  :a/u {:db/ident :a/u
                        :db/cardinality {:db/ident :db.cardinality/many}
                        :db/valueType {:db/ident :db.type/ref}}
                  :a/v {:db/ident :a/v
                        :db/cardinality {:db/ident :db.cardinality/many}
                        :db/valueType {:db/ident :db.type/string}}
                  :a/x {:db/ident :a/x
                        :db/cardinality {:db/ident :db.cardinality/one}
                        :db/valueType {:db/ident :db.type/ref}}
                  :a/y {:db/ident :a/y
                        :db/cardinality {:db/ident :db.cardinality/one}
                        :db/valueType {:db/ident :db.type/string}}
                  :a/z {:db/ident :a/z
                        :db/cardinality {:db/ident :db.cardinality/one}
                        :db/valueType {:db/ident :db.type/string}}
                  :b/x {:db/ident :b/x
                        :db/cardinality {:db/ident :db.cardinality/one}
                        :db/valueType {:db/ident :db.type/string}}
                  :b/y {:db/ident :b/y
                        :db/cardinality {:db/ident :db.cardinality/one}
                        :db/valueType {:db/ident :db.type/string}}
                  :e/a-one {:db/ident :e/a-one
                            :db/cardinality {:db/ident :db.cardinality/one}
                            :db/valueType {:db/ident :db.type/ref}}
                  :e/a-many {:db/ident :e/a-many
                             :db/cardinality {:db/ident :db.cardinality/many}
                             :db/valueType {:db/ident :db.type/ref}}})

(defn build-ctx [fiddle request result]
  {:route [nil [(->ThinEntity test-dbname 1)]]
   :hypercrud.browser/fiddle (r/atom fiddle)
   :hypercrud.browser/request (r/atom request)
   :hypercrud.browser/result (r/atom result)
   :hypercrud.browser/schemas (r/atom {test-dbname test-schema})})

#?(:clj
   (defmacro test-defined-pull [fiddle pull->request]
     (let [pull ''[:db/id
                   [:a/a :as "A"]
                   [:a/j :limit 1]
                   [:a/k :default "k default"]
                   [limit :a/v 1]
                   {[:a/t :as "T"] [*]
                    [limit :a/u nil] [*]
                    :a/x [:db/id :b/x]}
                   [default :a/y "a/y default"]
                   :a/z]]
       (macroexpand
         `(let [attributes# (->> @(auto-fields (build-ctx ~fiddle (~pull->request ~pull) nil))
                                 (mapcat ::field/children)
                                 (mapv ::field/path-segment))]
            (is (~'= [:a/a :a/j :a/k :a/v :a/t :a/u :a/x :a/y :a/z] attributes#)))))))

#?(:clj
   (defmacro test-splat [fiddle pull->request result-builder]
     (macroexpand
       `(let [result# (~result-builder [{:db/id 1
                                         :a/y "qwerty"
                                         :a/z "asdf"}
                                        {:db/id 2
                                         :a/x {:db/id 21 :b/x "hjkl"}
                                         :a/z "zxcv"}])
              attributes# (->> @(auto-fields (build-ctx ~fiddle (~pull->request [(symbol "*")]) result#))
                               (mapcat ::field/children)
                               (mapv ::field/path-segment)
                               (into #{}))]
          ; cant test order with splat
          (is (~'= #{:a/x :a/y :a/z (symbol "*")} attributes#))))))

#?(:clj
   (defmacro test-partial-splat [fiddle pull->request result-builder]
     (let [pull ''[:a/a
                   *
                   (limit :a/v 1)
                   {(default :a/s {}) [*]
                    [:a/t :as "T"] [*]
                    (limit :a/u 2) [*]
                    :a/x [*]}
                   (default :a/z "a/z default")]]
       (macroexpand
         `(let [result# (~result-builder [{:db/id 1
                                           :a/k "uiop"
                                           :a/s {}
                                           "T" {}
                                           :a/u [{:db/id 31 :b/y "fghj"}
                                                 {:db/id 32 :b/y "qazwsx"}]
                                           :a/z "asdf"}
                                          {:db/id 2
                                           :a/j "qwerty"
                                           :a/s {}
                                           :a/v ["vbnm"]
                                           :a/x {:db/id 21 :b/x "hjkl"}
                                           :a/z "zxcv"}])
                attributes# (->> @(auto-fields (build-ctx ~fiddle (~pull->request ~pull) result#))
                                 (mapcat ::field/children)
                                 (mapv ::field/path-segment))]
            ; can only test order of defined attributes in relation to splat
            (is (~'= :a/a (first attributes#)))
            (is (~'= :a/z (last attributes#)))
            (is (~'= (count attributes#) (count (into #{} attributes#))))
            (is (~'= #{:a/a :a/j :a/k :a/s :a/t :a/u :a/v :a/x :a/z (symbol "*")} (into #{} attributes#))))))))

#?(:clj
   (defmacro pull->attr-tests [fiddle pull->request result-builder]
     (macroexpand
       `(do (test-defined-pull ~fiddle ~pull->request)
            (test-splat ~fiddle ~pull->request ~result-builder)
            (test-partial-splat ~fiddle ~pull->request ~result-builder)))))

(deftest blank []
  (is (= [] @(auto-fields {:hypercrud.browser/schemas (r/atom nil)
                           :hypercrud.browser/fiddle (r/atom {:fiddle/type :blank})}))))

(deftest entity []
  (pull->attr-tests {:fiddle/type :entity
                     :fiddle/pull-database test-dbname}
                    #(->EntityRequest 1 nil (->DbVal nil nil) %)
                    (partial apply merge)))

(deftest entity-attr-one []
  (pull->attr-tests {:fiddle/type :entity
                     :fiddle/pull-database test-dbname}
                    #(->EntityRequest 1 :e/a-one (->DbVal nil nil) %)
                    (partial apply merge)))

(deftest entity-attr-many []
  (pull->attr-tests {:fiddle/type :entity
                     :fiddle/pull-database test-dbname}
                    #(->EntityRequest 1 :e/a-many (->DbVal nil nil) %)
                    identity))

(deftest query-rel []
  (pull->attr-tests {:fiddle/type :query}
                    #(->QueryRequest [:find (list 'pull '?e %) :in '$ '?e] {"$" nil "?e" 1})
                    (partial map vector)))

(deftest query-coll []
  (pull->attr-tests {:fiddle/type :query}
                    #(->QueryRequest [:find [(list 'pull '?e %) '...] :in '$ '?e] {"$" nil "?e" 1})
                    identity))

(deftest query-tuple []
  (pull->attr-tests {:fiddle/type :query}
                    #(->QueryRequest [:find [(list 'pull '?e %)] :in '$ '?e] {"$" nil "?e" 1})
                    (comp vector (partial apply merge))))

(deftest query-scalar []
  (pull->attr-tests {:fiddle/type :query}
                    #(->QueryRequest [:find (list 'pull '?e %) '. :in '$ '?e] {"$" nil "?e" 1})
                    (partial apply merge)))
