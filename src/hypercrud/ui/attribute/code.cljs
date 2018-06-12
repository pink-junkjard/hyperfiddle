(ns hypercrud.ui.attribute.code
  (:require [contrib.datomic-tx :as tx]
            [contrib.reactive :as r]
            [contrib.string :refer [empty->nil]]
            [contrib.ui :refer [code-block code-inline-block]]))


(defn ^:export code [value ctx props]
  (let [props (update props :read-only #(or % (nil? @(r/cursor (:cell-data ctx) [:db/id]))))
        change! (fn [%]
                  (let [tx (tx/update-entity-attr @(:cell-data ctx) @(:hypercrud.browser/fat-attribute ctx) (empty->nil %))]
                    ((:user-with! ctx) tx)))]
    [:div
     (let [control (case (:hyperfiddle.ui/layout ctx :hyperfiddle.ui.layout/block)
                     :hyperfiddle.ui.layout/block code-block
                     :hyperfiddle.ui.layout/table code-inline-block)]
       ; backwards args - props last
       [control props value change!])]))
