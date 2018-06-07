(ns hypercrud.ui.attribute.markdown-editor
  (:require [contrib.datomic-tx :as tx]
            [contrib.reactive :as r]
            [contrib.string :refer [empty->nil]]
            [contrib.ui :refer [code-block code-inline-block]]
            [hypercrud.ui.control.link-controls :as link-controls]))


(defn ^:export markdown-editor [value ctx props]
  (let [path [(:fe-pos ctx) (:hypercrud.browser/attribute ctx)]
        props (update props :read-only #(or % (nil? @(r/cursor (:cell-data ctx) [:db/id]))))
        change! #((:user-with! ctx) (tx/update-entity-attr @(:cell-data ctx) @(:hypercrud.browser/fat-attribute ctx) (empty->nil %)))]
    [:div.value
     [:div.anchors (link-controls/anchors path true ctx nil)]
     (let [widget (case (:hyperfiddle.ui/layout ctx :hyperfiddle.ui.layout/block)
                    :hyperfiddle.ui.layout/block code-block
                    :hyperfiddle.ui.layout/inline-block code-inline-block
                    :hyperfiddle.ui.layout/table code-inline-block)
           props (assoc props :mode "markdown" :lineWrapping true)]
       [widget props value change!])                        ; backwards args - props last
     (link-controls/iframes path true ctx)]))
