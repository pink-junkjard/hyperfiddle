(ns hyperfiddle.ide.fiddles.schema_test
  (:require [clojure.test :refer [deftest is]]
            [contrib.reactive :as r]
            [contrib.string :refer [slow-pprint-str]]
    #?(:cljs [hypercrud.browser.browser-ui-test :refer [test-renderer-str]])
            [hyperfiddle.ide.fiddles.schema :as schema]))

#?(:cljs
   (deftest schema-renderer []
     (let [ctx {:hypercrud.browser/attribute (r/atom nil)
                :hypercrud.browser/fiddle (r/atom nil)
                :hypercrud.browser/find-element (r/atom nil)
                :hypercrud.browser/links (r/atom nil)
                :hypercrud.browser/ordered-fes (r/atom [])
                :hypercrud.browser/request (r/atom nil)
                :hypercrud.browser/result (r/atom [])
                :relations (r/atom [])
                :hypercrud.browser/schema (r/atom nil)
                :hypercrud.browser/schemas (r/atom nil)}]
       ; just test it renderers something
       (is (not (nil? (test-renderer-str (:fiddle/renderer (schema/schema "$")) ctx)))))))

#?(:cljs
   (deftest db-attribute-renderer []
     (let [ctx {:hypercrud.browser/attribute (r/atom nil)
                :hypercrud.browser/fiddle (r/atom nil)
                :hypercrud.browser/find-element (r/atom nil)
                :hypercrud.browser/links (r/atom nil)
                :hypercrud.browser/ordered-fes (r/atom [])
                :hypercrud.browser/request (r/atom nil)
                :hypercrud.browser/result (r/atom nil)
                :relation (r/atom nil)
                :hypercrud.browser/schema (r/atom nil)
                :hypercrud.browser/schemas (r/atom nil)}]
       ; just test it renderers something
       (is (not (nil? (test-renderer-str (:fiddle/renderer (schema/db-attribute-edit "$")) ctx)))))))
