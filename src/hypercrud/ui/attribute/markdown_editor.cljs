(ns hypercrud.ui.attribute.markdown-editor
  (:require [contrib.datomic-tx :as tx]
            [contrib.reactive :as r]
            [contrib.string :refer [empty->nil]]
            [contrib.ui :refer [code-block code-inline-block]]))


(defn ^:export markdown-editor [value ctx props]
  (let [path [(:fe-pos ctx) (:hypercrud.browser/attribute ctx)]
        props (update props :read-only #(or % (nil? @(r/cursor (:cell-data ctx) [:db/id]))))
        change! #((:user-with! ctx) (tx/update-entity-attr @(:cell-data ctx) @(:hypercrud.browser/fat-attribute ctx) (empty->nil %)))]
    [:div
     (let [widget (case (:hyperfiddle.ui/layout ctx :hyperfiddle.ui.layout/block)
                    :hyperfiddle.ui.layout/block code-block
                    :hyperfiddle.ui.layout/inline-block code-inline-block
                    :hyperfiddle.ui.layout/table code-inline-block)
           props (assoc props :mode "markdown" :lineWrapping true)]
       ; backwards args - props last
       [widget props value change!])]))
