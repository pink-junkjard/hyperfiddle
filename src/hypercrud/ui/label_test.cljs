(ns hypercrud.ui.label-test
  (:require [cljs.test :refer-macros [deftest is]]
            [hypercrud.ui.label :refer [fqn->name]]))


(defn renderer-1 "(fn [_ _ ctx]\n  (let [ident @(contrib.reactive/cursor (:cell-data ctx) [:domain/ident])\n        href (str \"http://\" ident \".\" (:hyperfiddle-hostname ctx))]\n    [:a {:href href} href]))")

(deftest fqn->name-1
  []
  (is (= (fqn->name renderer-1) "(fn [_ _ ctx]"))
  (is (= (fqn->name "hypercrud.ui.attribute.code/code") "code"))
  (is (= (fqn->name "hypercrud.ui.attribute.code.code") "code")))
