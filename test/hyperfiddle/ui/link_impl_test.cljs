(ns hyperfiddle.ui.link-impl-test
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is]]
            [contrib.reactive :as r]
            [hyperfiddle.ui.link-impl :refer [contextual-links options-processor]]))


(def mock-links
  (for [rel [:my/link :options]
        path [nil
              ":body"
              ":head 0"
              ":body 0"
              ":head 0 :some/attr"
              ":body 0 :some/attr"]
        render-inline? [true false]
        managed? [true false]]
    {:link/rel rel
     :link/path path
     :link/render-inline? render-inline?
     :link/managed? managed?}))

(deftest x []
  (is (= (set (for [rel [:my/link :options]]
                {:link/rel rel
                 :link/path ":body"
                 :link/render-inline? true
                 :link/managed? false}))
         (set (contextual-links [:body] true (r/atom mock-links) nil))))

  (is (= (apply set/union (for [rel [:my/link :options]]
                            #{{:link/rel rel
                               :link/path ":head 0"
                               :link/render-inline? false
                               :link/managed? true}
                              {:link/rel rel
                               :link/path ":head 0"
                               :link/render-inline? false
                               :link/managed? false}
                              {:link/rel rel
                               :link/path ":head 0"
                               :link/render-inline? true
                               :link/managed? true}}))
         (into #{} (contextual-links [:head 0] false (r/atom mock-links) nil))))

  (is (= (set [{:link/rel :my/link
                :link/path ":head 0 :some/attr"
                :link/render-inline? true
                :link/managed? false}])
         (set (contextual-links [:head 0 :some/attr] true (r/atom mock-links) options-processor)))))
