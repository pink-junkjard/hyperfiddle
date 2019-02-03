(ns hypercrud.browser.field-test
  #?(:cljs (:require-macros [hypercrud.browser.field-test :refer [pull->attr-tests test-defined-pull test-partial-splat test-splat]]))
  (:require [clojure.test :refer [deftest is testing]]
            [contrib.data :as data]
            [contrib.reactive :as r]
            [hypercrud.browser.context :as context]
            [hypercrud.browser.field :as field :refer [auto-field infer-attrs]]
            [hypercrud.types.DbRef :refer [->DbRef]]
            [hypercrud.types.EntityRequest :refer [->EntityRequest]]
            [hypercrud.types.QueryRequest :refer [->QueryRequest]]
            [hyperfiddle.runtime :as runtime]))


(deftest test-infer-attr []
  (let [data {:a/x 1
              :a/y [{:b/x 2
                     :b/y [{:asdf 1}
                           {:qwerty 3}]
                     :b/z {:c/a 1}}
                    {:b/x 3
                     :b/y [{:asdf2 1}]
                     :b/z {:c/b 2}}]
              :a/z {:c/x 5}}]
    (doseq [[get-values expected] [[[] #{:a/x :a/y :a/z}]
                                   [[:a/y] #{:b/x :b/y :b/z}]
                                   [[:a/y :b/y] #{:asdf :qwerty :asdf2}]
                                   [[:a/y :b/z] #{:c/a :c/b}]
                                   [[:a/z] #{:c/x}]]]
      (is (= expected (infer-attrs data get-values)))
      (is (= expected (infer-attrs [data data] get-values))) ; test resultsets
      )))

(def test-schema
  (->> [{:db/ident :a/j
         :db/cardinality {:db/ident :db.cardinality/one}
         :db/valueType {:db/ident :db.type/string}}
        {:db/ident :a/k
         :db/cardinality {:db/ident :db.cardinality/one}
         :db/valueType {:db/ident :db.type/string}}
        {:db/ident :a/s
         :db/cardinality {:db/ident :db.cardinality/one}
         :db/valueType {:db/ident :db.type/ref}}
        {:db/ident :a/t
         :db/cardinality {:db/ident :db.cardinality/one}
         :db/valueType {:db/ident :db.type/ref}}
        {:db/ident :a/u
         :db/cardinality {:db/ident :db.cardinality/many}
         :db/valueType {:db/ident :db.type/ref}}
        {:db/ident :a/v
         :db/cardinality {:db/ident :db.cardinality/many}
         :db/valueType {:db/ident :db.type/string}}
        {:db/ident :a/x
         :db/cardinality {:db/ident :db.cardinality/one}
         :db/valueType {:db/ident :db.type/ref}}
        {:db/ident :a/y
         :db/cardinality {:db/ident :db.cardinality/one}
         :db/valueType {:db/ident :db.type/string}}
        {:db/ident :a/z
         :db/cardinality {:db/ident :db.cardinality/one}
         :db/valueType {:db/ident :db.type/string}}
        {:db/ident :a/comp-one
         :db/cardinality {:db/ident :db.cardinality/one}
         :db/valueType {:db/ident :db.type/ref}
         :db/isComponent true}
        {:db/ident :a/comp-many
         :db/cardinality {:db/ident :db.cardinality/one}
         :db/valueType {:db/ident :db.type/ref}
         :db/isComponent true}
        {:db/ident :b/x
         :db/cardinality {:db/ident :db.cardinality/one}
         :db/valueType {:db/ident :db.type/string}}
        {:db/ident :b/y
         :db/cardinality {:db/ident :db.cardinality/one}
         :db/valueType {:db/ident :db.type/string}}]
       (data/group-by-assume-unique :db/ident)))

(defn build-ctx [fiddle result]                             ; this is starting to look a lot like base/process-results
  {:hypercrud.browser/fiddle (r/atom fiddle)
   :hypercrud.browser/data (r/atom result)
   :peer (let [state (r/atom {::runtime/partitions {nil {:schemas {"$" test-schema}}}})]
           (reify
             runtime/State
             (runtime/state [_] state)
             (runtime/state [_ path] (r/cursor state path))))})

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
         `(let [attributes# (->> @(auto-field (r/atom (~pull->request ~pull)) (build-ctx ~fiddle nil))
                                 ::field/children
                                 (mapcat (fn [field#]
                                           (if (context/find-element-segment? (::field/path-segment field#))
                                             (::field/children field#)
                                             [field#])))
                                 (mapv ::field/path-segment))]
            (is (~'= [:db/id :a/a :a/j :a/k :a/v :a/t :a/u :a/x :a/y :a/z] attributes#)))))))

#?(:clj
   (defmacro test-splat [fiddle pull->request result-builder]
     (macroexpand
       `(let [result# (~result-builder [{:db/id 1
                                         :a/comp-one {:db/id 15 :b/x "poiu"}
                                         :a/comp-many [{:db/id 16 :b/x "mnbv"}
                                                       {:db/id 17 :b/y "gfds"}]
                                         :a/y "qwerty"
                                         :a/z "asdf"}
                                        {:db/id 2
                                         :a/comp-one {:db/id 25 :b/y "lkjh"}
                                         :a/comp-many [{:db/id 26 :a/j "bvcx"}
                                                       {:db/id 27 :b/y "trew"}]
                                         :a/x {:db/id 21 :b/x "hjkl"}
                                         :a/z "zxcv"}])
              attr-level-fields# (->> @(auto-field (r/atom (~pull->request [(symbol "*")])) (build-ctx ~fiddle result#))
                                      ::field/children
                                      (mapcat (fn [field#]
                                                (if (context/find-element-segment? (::field/path-segment field#))
                                                  (::field/children field#)
                                                  [field#]))))]
          ; cant test order with splat
          (is (~'= #{:db/id :a/x :a/y :a/z :a/comp-one :a/comp-many (symbol "*")}
                (->> attr-level-fields#
                     (mapv ::field/path-segment)
                     (into #{}))))

          ; ensure sure splat -> nested components works
          (is (~'= #{:db/id :b/x :b/y}
                (->> attr-level-fields#
                     (filter (comp #(= % :a/comp-one) ::field/path-segment))
                     first
                     ::field/children
                     (mapv ::field/path-segment)
                     (into #{}))))

          ; ensure sure splat -> nested component works
          (is (~'= #{:db/id :a/j :b/x :b/y}
                (->> attr-level-fields#
                     (filter (comp #(= % :a/comp-many) ::field/path-segment))
                     first
                     ::field/children
                     (mapv ::field/path-segment)
                     (into #{}))))
          ))))

#?(:clj
   (defmacro test-partial-splat [fiddle pull->request result-builder]
     (let [pull ''[:a/a
                   [:a/comp-one :as "comp-one"]             ; rest of nesting comes for free on component
                   :a/comp-many                             ; rest of nesting comes for free on component
                   *
                   (limit :a/v 1)
                   {(default :a/s {}) [*]
                    [:a/t :as "T"] [*]
                    (limit :a/u 2) [*]
                    :a/x [*]}
                   (default :a/z "a/z default")]]
       (macroexpand
         `(let [result# (~result-builder [{:db/id 1
                                           "comp-one" {:db/id 15 :b/x "poiu"}
                                           :a/comp-many [{:db/id 16 :b/x "mnbv"}
                                                         {:db/id 17 :b/y "gfds"}]
                                           :a/k "uiop"
                                           :a/s {}
                                           "T" {:db/id 12 :b/x "tyui"}
                                           :a/u [{:db/id 31 :b/y "fghj"}
                                                 {:db/id 32 :b/y "qazwsx"}]
                                           :a/z "asdf"}
                                          {:db/id 2
                                           "comp-one" {:db/id 25 :b/y "lkjh"}
                                           :a/comp-many [{:db/id 26 :a/j "bvcx"}
                                                         {:db/id 27 :b/y "trew"}]
                                           :a/j "qwerty"
                                           :a/s {}
                                           "T" {:db/id 12 :b/y "dfgh"}
                                           :a/v ["vbnm"]
                                           :a/x {:db/id 21 :b/x "hjkl"}
                                           :a/z "zxcv"}])
                attr-level-fields# (->> @(auto-field (r/atom (~pull->request ~pull)) (build-ctx ~fiddle result#))
                                        ::field/children
                                        (mapcat (fn [field#]
                                                  (if (context/find-element-segment? (::field/path-segment field#))
                                                    (::field/children field#)
                                                    [field#]))))
                attributes# (mapv ::field/path-segment attr-level-fields#)]
            ; can only test order of defined attributes in relation to splat
            (is (~'= :a/a (first attributes#)))
            (is (~'= :a/z (last attributes#)))
            (is (~'= (count attributes#) (count (into #{} attributes#))))
            (is (~'= #{:db/id :a/a :a/j :a/k :a/s :a/t :a/u :a/v :a/x :a/z :a/comp-one :a/comp-many (symbol "*")} (into #{} attributes#)))
            (is (~'= #{:db/id :b/x :b/y (symbol "*")}
                  (->> attr-level-fields#
                       (filter (comp #(= % :a/t) ::field/path-segment))
                       first
                       ::field/children
                       (mapv ::field/path-segment)
                       (into #{}))))
            (is (~'= #{:db/id :b/x :b/y}
                  (->> attr-level-fields#
                       (filter (comp #(= % :a/comp-one) ::field/path-segment))
                       first
                       ::field/children
                       (mapv ::field/path-segment)
                       (into #{}))))
            (is (~'= #{:db/id :a/j :b/x :b/y}
                  (->> attr-level-fields#
                       (filter (comp #(= % :a/comp-many) ::field/path-segment))
                       first
                       ::field/children
                       (mapv ::field/path-segment)
                       (into #{}))))
            )))))

#?(:clj
   (defmacro pull->attr-tests [fiddle pull->request result-builder]
     (macroexpand
       `(do (test-defined-pull ~fiddle ~pull->request)
            (test-splat ~fiddle ~pull->request ~result-builder)
            (test-partial-splat ~fiddle ~pull->request ~result-builder)))))

(deftest blank []
  (is (nil? @(auto-field nil (build-ctx {:fiddle/type :blank} nil)))))

(letfn [(f [a b]
          (cond
            (map? a) (merge-with f a b)
            (vector? a) (concat a b)
            :else b))]
  (def merge-into-one (partial apply merge-with f)))

(deftest entity []
  (pull->attr-tests {:fiddle/type :entity
                     :fiddle/pull-database "$"}
                    #(->EntityRequest 1 (->DbRef "$" nil) %)
                    merge-into-one))

(deftest query-rel []
  (pull->attr-tests {:fiddle/type :query}
                    #(->QueryRequest [:find (list 'pull '?e %) :in '$ '?e] [(->DbRef "$" nil) 1])
                    (partial map vector)))

(deftest query-coll []
  (pull->attr-tests {:fiddle/type :query}
                    #(->QueryRequest [:find [(list 'pull '?e %) '...] :in '$ '?e] [(->DbRef "$" nil) 1])
                    identity))

(deftest query-tuple []
  (pull->attr-tests {:fiddle/type :query}
                    #(->QueryRequest [:find [(list 'pull '?e %)] :in '$ '?e] [(->DbRef "$" nil) 1])
                    (comp vector merge-into-one)))

(deftest query-scalar []
  (pull->attr-tests {:fiddle/type :query}
                    #(->QueryRequest [:find (list 'pull '?e %) '. :in '$ '?e] [(->DbRef "$" nil) 1])
                    merge-into-one))
