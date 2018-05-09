(ns hypercrud.browser.find-element-test
  #?(:cljs (:require-macros [hypercrud.browser.find-element-test :refer [test-defined-pull test-partial-splat test-splat]]))
  (:require [clojure.test :refer [deftest is]]
            [contrib.reactive :as r]
            [hypercrud.browser.find-element :refer [auto-find-elements]]
            [hypercrud.types.DbVal :refer [->DbVal]]
            [hypercrud.types.EntityRequest :refer [->EntityRequest]]
            [hypercrud.types.QueryRequest :refer [->QueryRequest]]
            [hypercrud.types.ThinEntity :refer [->ThinEntity]]))


(def test-dbname "test")
(def test-schema {:a/x {:db/ident :a/x
                        :db/cardinality {:db/ident :db.cardinality/one}
                        :db/valueType {:db/ident :db.type/string}}
                  :a/j {:db/ident :a/j
                        :db/cardinality {:db/ident :db.cardinality/one}
                        :db/valueType {:db/ident :db.type/string}}
                  :a/k {:db/ident :a/k
                        :db/cardinality {:db/ident :db.cardinality/one}
                        :db/valueType {:db/ident :db.type/string}}
                  :a/y {:db/ident :a/y
                        :db/cardinality {:db/ident :db.cardinality/one}
                        :db/valueType {:db/ident :db.type/string}}
                  :a/z {:db/ident :a/z
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
     (macroexpand
       `(let [attributes# (->> @(auto-find-elements (build-ctx ~fiddle (~pull->request [:db/id :a/x :a/y :a/z]) nil))
                               (mapcat :fields)
                               (mapv :attribute))]
          (is (~'= [:a/x :a/y :a/z] attributes#))))))

#?(:clj
   (defmacro test-splat [fiddle pull->request result]
     (macroexpand
       `(let [attributes# (->> @(auto-find-elements (build-ctx ~fiddle (~pull->request [(symbol "*")]) ~result))
                               (mapcat :fields)
                               (mapv :attribute)
                               (into #{}))]
          ; cant test order with splat
          (is (~'= #{:a/x :a/y :a/z} attributes#))))))

#?(:clj
   (defmacro test-partial-splat [fiddle pull->request result]
     (macroexpand
       `(let [attributes# (->> @(auto-find-elements (build-ctx ~fiddle (~pull->request [:a/a (symbol "*") :a/z]) ~result))
                               (mapcat :fields)
                               (mapv :attribute))]
          ; can only test order of defined attributes in relation to splat
          (is (~'= (first attributes#) :a/a))
          (is (~'= (last attributes#) :a/z))
          (is (~'= #{:a/a :a/j :a/k :a/z} (into #{} attributes#)))))))

(deftest blank []
  (is (= [] @(auto-find-elements {:hypercrud.browser/fiddle (r/atom {:fiddle/type :blank})}))))

(deftest entity []
  (let [pull->req #(->EntityRequest 1 nil (->DbVal nil nil) %)]
    (test-defined-pull {:fiddle/type :entity} pull->req)
    (test-splat {:fiddle/type :entity} pull->req {:db/id 1
                                                  :a/x "asdf"
                                                  :a/y "qwerty"
                                                  :a/z "zxcv"})
    (test-partial-splat {:fiddle/type :entity} pull->req {:db/id 1
                                                          :a/j "qwerty"
                                                          :a/k "qwerty"
                                                          :a/z "zxcv"})))

(deftest entity-attr-one []
  (let [pull->req #(->EntityRequest 1 :e/a-one (->DbVal nil nil) %)]
    (test-defined-pull {:fiddle/type :entity} pull->req)
    (test-splat {:fiddle/type :entity} pull->req {:db/id 1
                                                  :a/x "asdf"
                                                  :a/y "qwerty"
                                                  :a/z "zxcv"})
    (test-partial-splat {:fiddle/type :entity} pull->req {:db/id 1
                                                          :a/j "qwerty"
                                                          :a/k "qwerty"
                                                          :a/z "zxcv"})))

(deftest entity-attr-many []
  (let [pull->req #(->EntityRequest 1 :e/a-many (->DbVal nil nil) %)]
    (test-defined-pull {:fiddle/type :entity} pull->req)
    (test-splat {:fiddle/type :entity} pull->req [{:db/id 1
                                                   :a/x "asdf"
                                                   :a/y "qwerty"
                                                   :a/z "zxcv"}
                                                  {:db/id 1
                                                   :a/x "asdf"
                                                   :a/z "zxcv"}])
    (test-partial-splat {:fiddle/type :entity} pull->req [{:db/id 1
                                                           :a/k "qwerty"
                                                           :a/z "zxcv"}
                                                          {:db/id 2
                                                           :a/j "qwerty"
                                                           :a/z "zxcv"}])))

(deftest query-rel []
  (let [pull->req #(->QueryRequest [:find (list 'pull '?e %) :in '$ '?e] {"$" nil "?e" 1})]
    (test-defined-pull {:fiddle/type :query} pull->req)
    (test-splat {:fiddle/type :query} pull->req [[{:db/id 1
                                                   :a/x "asdf"
                                                   :a/y "qwerty"
                                                   :a/z "zxcv"}]])
    (test-partial-splat {:fiddle/type :query} pull->req [[{:db/id 1
                                                           :a/k "qwerty"
                                                           :a/z "zxcv"}]
                                                         [{:db/id 2
                                                           :a/j "qwerty"
                                                           :a/z "zxcv"}]])))

(deftest query-coll []
  (let [pull->req #(->QueryRequest [:find [(list 'pull '?e %) '...] :in '$ '?e] {"$" nil "?e" 1})]
    (test-defined-pull {:fiddle/type :query} pull->req)
    (test-splat {:fiddle/type :query} pull->req [{:db/id 1
                                                  :a/x "asdf"
                                                  :a/y "qwerty"
                                                  :a/z "zxcv"}])
    (test-partial-splat {:fiddle/type :query} pull->req [{:db/id 1
                                                          :a/k "qwerty"
                                                          :a/z "zxcv"}
                                                         {:db/id 2
                                                          :a/j "qwerty"
                                                          :a/z "zxcv"}])))

(deftest query-tuple []
  (let [pull->req #(->QueryRequest [:find [(list 'pull '?e %)] :in '$ '?e] {"$" nil "?e" 1})]
    ; https://github.com/hyperfiddle/hyperfiddle.net/issues/276
    #_(test-defined-pull {:fiddle/type :query} pull->req)
    (test-splat {:fiddle/type :query} pull->req [{:db/id 1
                                                  :a/x "asdf"
                                                  :a/y "qwerty"
                                                  :a/z "zxcv"}])
    (test-partial-splat {:fiddle/type :query} pull->req [{:db/id 1
                                                          :a/j "qwerty"
                                                          :a/k "qwerty"
                                                          :a/z "zxcv"}])))

(deftest query-scalar []
  (let [pull->req #(->QueryRequest [:find (list 'pull '?e %) '. :in '$ '?e] {"$" nil "?e" 1})]
    (test-defined-pull {:fiddle/type :query} pull->req)
    (test-splat {:fiddle/type :query} pull->req {:db/id 1
                                                 :a/x "asdf"
                                                 :a/y "qwerty"
                                                 :a/z "zxcv"})
    (test-partial-splat {:fiddle/type :query} pull->req {:db/id 1
                                                         :a/j "qwerty"
                                                         :a/k "qwerty"
                                                         :a/z "zxcv"})))
