(ns hypercrud.ui.attribute.code
  (:require [hypercrud.client.tx :as tx]
            [hypercrud.ui.control.code :as code]
            [hypercrud.ui.control.link-controls :as link-controls]))


(defn ^:export code [field props ctx]
  (let [path [(:fe-pos ctx) (:hypercrud.browser/attribute ctx)]
        change! #((:user-with! ctx) (tx/update-entity-attr @(:cell-data ctx) @(:hypercrud.browser/fat-attribute ctx) %))]
    ;^{:key ident}
    [:div.value
     [:div.anchors (link-controls/render-nav-cmps path true ctx)]
     (let [control (case (:layout ctx) :block code/code-block*
                                       :inline-block code/code-inline-block*
                                       :table code/code-inline-block*)]
       [control props @(:value ctx) change!])               ; backwards args - props last
     (link-controls/render-inline-links path true ctx)]))
