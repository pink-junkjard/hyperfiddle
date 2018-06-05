(ns hypercrud.ui.attribute.code
  (:require [contrib.datomic-tx :as tx]
            [contrib.reactive :as r]
            [contrib.string :refer [empty->nil]]
            [contrib.ui :refer [code-block code-inline-block]]
            [hypercrud.ui.control.link-controls :as link-controls]))


(defn ^:export code [value ctx props]
  (let [path [(:fe-pos ctx) (:hypercrud.browser/attribute ctx)]
        props (update props :read-only #(or % (nil? @(r/cursor (:cell-data ctx) [:db/id]))))
        change! (fn [%]
                  (let [tx (tx/update-entity-attr @(:cell-data ctx) @(:hypercrud.browser/fat-attribute ctx) (empty->nil %))]
                    ((:user-with! ctx) tx)))]
    [:div
     [:div.anchors (link-controls/anchors path true ctx)]
     (let [control (case (:hyperfiddle.ui/layout ctx :hyperfiddle.ui.layout/block)
                     :hyperfiddle.ui.layout/block code-block
                     :hyperfiddle.ui.layout/inline-block code-inline-block
                     :hyperfiddle.ui.layout/table code-inline-block)]
       [control props value change!])               ; backwards args - props last
     (link-controls/iframes path true ctx)]))
