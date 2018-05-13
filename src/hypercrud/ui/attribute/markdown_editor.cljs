(ns hypercrud.ui.attribute.markdown-editor
  (:require [contrib.datomic-tx :as tx]
            [contrib.string :refer [empty->nil]]
            [contrib.ui :refer [code-block code-inline-block]]
            [hypercrud.ui.control.link-controls :as link-controls]))


(defn ^:export markdown-editor [field props ctx]
  (let [path [(:fe-pos ctx) (:hypercrud.browser/attribute ctx)]
        change! #((:user-with! ctx) (tx/update-entity-attr @(:cell-data ctx) @(:hypercrud.browser/fat-attribute ctx) (empty->nil %)))]
    ;^{:key ident}
    [:div.value
     [:div.anchors (link-controls/anchors path true ctx)]
     (let [widget (case (:layout ctx) :block code-block
                                      :inline-block code-inline-block
                                      :table code-inline-block)
           props (assoc props :mode "markdown" :lineWrapping true)]
       [widget props @(:value ctx) change!])                 ; backwards args - props last
     (link-controls/iframes path true ctx)]))
