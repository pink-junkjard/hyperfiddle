(ns hypercrud.ui.control.link-controls-test
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is]]
            [contrib.reactive :as r]
            [hypercrud.browser.link :as link]
            [hypercrud.ui.control.link-controls :as link-controls]))


(def mock-links
  (for [rel [:my/link :options]
        path [nil "0" "0 :some/attr"]
        dependent? [true false]
        render-inline? [true false]
        managed? [true false]]
    {:link/rel rel
     :link/path path
     :link/dependent? dependent?
     :link/render-inline? render-inline?
     :link/managed? managed?}))

(deftest x []
  (is (= (set (for [rel [:my/link :options]]
                {:link/rel rel
                 :link/path nil
                 :link/dependent? true
                 :link/render-inline? true
                 :link/managed? false}))
         (set (link-controls/ui-contextual-links :body nil nil true (r/atom mock-links) nil))))

  (is (= (apply set/union (for [rel [:my/link :options]]
                            #{{:link/rel rel
                               :link/path "0"
                               :link/dependent? false
                               :link/render-inline? false
                               :link/managed? true}
                              {:link/rel rel
                               :link/path "0"
                               :link/dependent? false
                               :link/render-inline? false
                               :link/managed? false}
                              {:link/rel rel
                               :link/path "0"
                               :link/dependent? false
                               :link/render-inline? true
                               :link/managed? true}}))
         (into #{} (link-controls/ui-contextual-links :head 0 nil false (r/atom mock-links) nil))))

  (is (= (set [{:link/rel :my/link
                :link/path "0 :some/attr"
                :link/dependent? false
                :link/render-inline? true
                :link/managed? false}])
         (set (link-controls/ui-contextual-links :head 0 :some/attr true (r/atom mock-links) link/options-processor)))))
