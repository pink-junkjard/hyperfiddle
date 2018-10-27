(ns hyperfiddle.fiddle-test
  (:require
    [clojure.test :refer [deftest is]]
    [contrib.reactive :as r]
    #?(:cljs [hyperfiddle.ui])                              ; hack for circular dep hack
    [hyperfiddle.fiddle :refer [data-defaults fiddle-defaults]]))


(deftest test-data-defaults []
  (is (not (nil? (:fiddle/query (data-defaults {:fiddle/type :query})))))
  (is (not (nil? (:fiddle/pull (data-defaults {:fiddle/type :entity}))))))

#?(:cljs
   (deftest test-ui-defaults []
     (let [fiddle-val (fiddle-defaults {} nil)]
       (is (not (nil? (:fiddle/markdown fiddle-val))))
       (is (not (nil? (:fiddle/renderer fiddle-val)))))))


(def gender-field
  '{:hypercrud.browser.field/query
    {:qfind {:element {:source {:symbol $}, :variable {:symbol ?e}, :pattern {:value [:db/id :reg/email :reg/name :reg/age :reg/birthdate {:reg/gender [:db/ident]} {:reg/shirt-size [:db/ident]}]}}},
     :qwith nil,
     :qin [{:variable {:symbol $}}],
     :qwhere [{:source {}, :pattern [{:symbol ?e} {:value :reg/email} {:value "dustin@example.com"}]}]},
    :hypercrud.browser.field/cardinality :db.cardinality/one,
    :hypercrud.browser.field/children
    [{:hypercrud.browser.field/cardinality :db.cardinality/one,
      :hypercrud.browser.field/children nil,
      :hypercrud.browser.field/data-has-id? false,
      :hypercrud.browser.field/label "id",
      :hypercrud.browser.field/get-value :db/id,
      :hypercrud.browser.field/path-segment :db/id,
      :hypercrud.browser.field/source-symbol $}
     {:hypercrud.browser.field/cardinality :db.cardinality/one,
      :hypercrud.browser.field/children nil,
      :hypercrud.browser.field/data-has-id? false,
      :hypercrud.browser.field/label "email",
      :hypercrud.browser.field/get-value :reg/email,
      :hypercrud.browser.field/path-segment :reg/email,
      :hypercrud.browser.field/source-symbol $}
     {:hypercrud.browser.field/cardinality :db.cardinality/one,
      :hypercrud.browser.field/children nil,
      :hypercrud.browser.field/data-has-id? false,
      :hypercrud.browser.field/label "name",
      :hypercrud.browser.field/get-value :reg/name,
      :hypercrud.browser.field/path-segment :reg/name,
      :hypercrud.browser.field/source-symbol $}
     {:hypercrud.browser.field/cardinality :db.cardinality/one,
      :hypercrud.browser.field/children nil,
      :hypercrud.browser.field/data-has-id? false,
      :hypercrud.browser.field/label "age",
      :hypercrud.browser.field/get-value :reg/age,
      :hypercrud.browser.field/path-segment :reg/age,
      :hypercrud.browser.field/source-symbol $}
     {:hypercrud.browser.field/cardinality :db.cardinality/one,
      :hypercrud.browser.field/children nil,
      :hypercrud.browser.field/data-has-id? false,
      :hypercrud.browser.field/label "birthdate",
      :hypercrud.browser.field/get-value :reg/birthdate,
      :hypercrud.browser.field/path-segment :reg/birthdate,
      :hypercrud.browser.field/source-symbol $}
     {:hypercrud.browser.field/cardinality :db.cardinality/one,
      :hypercrud.browser.field/label "gender",
      :hypercrud.browser.field/get-value :reg/gender,
      :hypercrud.browser.field/path-segment :reg/gender,
      :hypercrud.browser.field/source-symbol $,
      :hypercrud.browser.field/children
      [{:hypercrud.browser.field/cardinality :db.cardinality/one,
        :hypercrud.browser.field/children nil,
        :hypercrud.browser.field/data-has-id? false,
        :hypercrud.browser.field/label "ident",
        :hypercrud.browser.field/get-value :db/ident,
        :hypercrud.browser.field/path-segment :db/ident,
        :hypercrud.browser.field/source-symbol $}],
      :hypercrud.browser.field/data-has-id? true}
     {:hypercrud.browser.field/cardinality :db.cardinality/one,
      :hypercrud.browser.field/label "shirt-size",
      :hypercrud.browser.field/get-value :reg/shirt-size,
      :hypercrud.browser.field/path-segment :reg/shirt-size,
      :hypercrud.browser.field/source-symbol $,
      :hypercrud.browser.field/children
      [{:hypercrud.browser.field/cardinality :db.cardinality/one,
        :hypercrud.browser.field/children nil,
        :hypercrud.browser.field/data-has-id? false,
        :hypercrud.browser.field/label "ident",
        :hypercrud.browser.field/get-value :db/ident,
        :hypercrud.browser.field/path-segment :db/ident,
        :hypercrud.browser.field/source-symbol $}],
      :hypercrud.browser.field/data-has-id? true}],
    :hypercrud.browser.field/data-has-id? true,
    :hypercrud.browser.field/get-value identity,
    :hypercrud.browser.field/label ?e,
    :hypercrud.browser.field/path-segment nil,
    :hypercrud.browser.field/source-symbol $})

