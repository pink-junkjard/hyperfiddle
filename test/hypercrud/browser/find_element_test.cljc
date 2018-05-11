(ns hypercrud.browser.find-element-test
  #?(:cljs (:require-macros [hypercrud.browser.find-element-test :refer [pull->attr-tests test-defined-pull test-partial-splat test-splat]]))
  (:require [clojure.test :refer [deftest is]]
            [contrib.reactive :as r]
            [hypercrud.browser.find-element :refer [auto-find-elements]]
            [hypercrud.types.DbVal :refer [->DbVal]]
            [hypercrud.types.EntityRequest :refer [->EntityRequest]]
            [hypercrud.types.QueryRequest :refer [->QueryRequest]]
            [hypercrud.types.ThinEntity :refer [->ThinEntity]]))


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
         `(let [attributes# (->> @(auto-find-elements (build-ctx ~fiddle (~pull->request ~pull) nil))
                                 (mapcat :fields)
                                 (mapv :attribute))]
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
              attributes# (->> @(auto-find-elements (build-ctx ~fiddle (~pull->request [(symbol "*")]) result#))
                               (mapcat :fields)
                               (mapv :attribute)
                               (into #{}))]
          ; cant test order with splat
          (is (~'= #{:a/x :a/y :a/z} attributes#))))))

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
                                           :a/u [{:db/id 31 :b/y "fghj"}
                                                 {:db/id 32 :b/y "qazwsx"}]
                                           :a/z "asdf"}
                                          {:db/id 2
                                           :a/j "qwerty"
                                           :a/s {}
                                           :a/v ["vbnm"]
                                           :a/x {:db/id 21 :b/x "hjkl"}
                                           :a/z "zxcv"}])
                attributes# (->> @(auto-find-elements (build-ctx ~fiddle (~pull->request ~pull) result#))
                                 (mapcat :fields)
                                 (mapv :attribute))]
            ; can only test order of defined attributes in relation to splat
            (is (~'= :a/a (first attributes#)))
            (is (~'= :a/z (last attributes#)))
            (is (~'= (count attributes#) (count (into #{} attributes#))))
            (is (~'= #{:a/a :a/j :a/k :a/s :a/t :a/u :a/v :a/x :a/z} (into #{} attributes#))))))))

#?(:clj
   (defmacro pull->attr-tests [fiddle pull->request result-builder]
     (macroexpand
       `(do (test-defined-pull ~fiddle ~pull->request)
            (test-splat ~fiddle ~pull->request ~result-builder)
            (test-partial-splat ~fiddle ~pull->request ~result-builder)))))

(deftest blank []
  (is (= [] @(auto-find-elements {:hypercrud.browser/fiddle (r/atom {:fiddle/type :blank})}))))

(deftest entity []
  (pull->attr-tests {:fiddle/type :entity}
                    #(->EntityRequest 1 nil (->DbVal nil nil) %)
                    (partial apply merge)))

(deftest entity-attr-one []
  (pull->attr-tests {:fiddle/type :entity}
                    #(->EntityRequest 1 :e/a-one (->DbVal nil nil) %)
                    (partial apply merge)))

(deftest entity-attr-many []
  (pull->attr-tests {:fiddle/type :entity}
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
