(ns hypercrud.browser.fiddle-test
  (:require
    [clojure.test :refer [deftest is]]
    [hypercrud.browser.fiddle :refer [data-defaults fiddle-defaults]]
    [hypercrud.types.Entity :refer [->Entity]]))


(deftest test-data-defaults []
  (is (not (nil? (:fiddle/query (data-defaults {:fiddle/type :query})))))
  (is (not (nil? (:fiddle/pull (data-defaults {:fiddle/type :entity}))))))

#?(:cljs
   (deftest test-ui-defaults []
     (let [fiddle-val (fiddle-defaults {})]
       (is (not (nil? (:fiddle/markdown fiddle-val))))
       (is (not (nil? (:fiddle/renderer fiddle-val)))))))
