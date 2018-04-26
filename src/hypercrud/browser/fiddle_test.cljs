(ns hypercrud.browser.fiddle-test
  (:require
    [clojure.test :refer [deftest is]]
    [contrib.reader]
    [hypercrud.browser.fiddle :refer [fiddle-defaults]]
    [hypercrud.types.Entity :refer [->Entity]]))


(deftest fiddle-defaults-1 []
  (def fiddle-val {:db/id 17592186045930, :fiddle/ident :tbd.11/completed, :fiddle/type :query})
  (def entity (->Entity nil fiddle-val))
  (is (not (nil? (-> fiddle-val fiddle-defaults :fiddle/renderer)))))
