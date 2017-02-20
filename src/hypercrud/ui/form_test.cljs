(ns hypercrud.ui.form-test
  (:require-macros [cljs.test :refer [deftest testing is]])
  (:require [cljs.test]
            [hypercrud.types :refer [->DbId]]
            [hypercrud.ui.form :refer [form]]))

(def colspec [:entity :db/id nil :entity :hypercrud/owner nil :entity :link/name nil :entity :link/renderer nil])

(partition 3 colspec)

(deftest test-form []

  (is (= 1
         2))

 )