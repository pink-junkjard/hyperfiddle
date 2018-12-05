(ns hyperfiddle.ui.docstring-test
  (:require
    [clojure.test :refer [deftest is]]
    [hyperfiddle.ui.docstring :refer [fqn->name]]))


(def renderer-1 "(fn [_ _ ctx]\n  (let [ident @(contrib.reactive/cursor (:cell-data ctx) [:domain/ident])\n        href (str \"http://\" ident \".\" (:hyperfiddle-hostname ctx))]\n    [:a {:href href} href]))")

(deftest fqn->name-1
  []
  (is (= (fqn->name renderer-1) "(fn [_ _ ctx]"))
  (is (= (fqn->name "hyperfiddle.ui.controls/code") "code"))
  (is (= (fqn->name "hyperfiddle.ui.controls/code") "code")))
