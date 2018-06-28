(ns hyperfiddle.io.rpc-router-test
  (:require
    [clojure.test :refer [deftest is]]
    [hyperfiddle.io.rpc-router :refer :all]))


(def global-basis [[#uri "datomic:free://datomic:4334/domains" 1776]
                   [#uri "datomic:free://datomic:4334/domains" 1776]
                   [#uri "datomic:free://datomic:4334/hyperfiddle-blog-source" 3183]
                   [#uri "datomic:free://datomic:4334/hyperfiddle-users" 34120]
                   [#uri "datomic:free://datomic:4334/root" 17048]])

(def global-basis-str "%22datomic:free:%2F%2Fdatomic:4334%2Fdomains%22,1776,%22datomic:free:%2F%2Fdatomic:4334%2Fdomains%22,1776,%22datomic:free:%2F%2Fdatomic:4334%2Fhyperfiddle-blog-source%22,3183,%22datomic:free:%2F%2Fdatomic:4334%2Fhyperfiddle-users%22,34120,%22datomic:free:%2F%2Fdatomic:4334%2Froot%22,17048")

(defn encode-basis-
  []
  (is (= (encode-basis global-basis) global-basis-str)))

(defn decode-basis-
  []
  (is (= (decode-basis global-basis-str) global-basis)))
