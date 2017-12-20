(ns hypercrud.ui.attribute.markdown-editor
  (:require [hypercrud.client.tx :as tx]
            [hypercrud.ui.control.code :as code]
            [hypercrud.ui.control.link-controls :as links]
            [hypercrud.browser.anchor :as link]))


(defn ^:export markdown-editor [field links props ctx]
  (let [my-links (link/links-lookup' links [(:fe-pos ctx) (-> ctx :attribute :db/ident)])
        change! #((:user-with! ctx) (tx/update-entity-attr (:cell-data ctx) (:attribute ctx) %))]
    ;^{:key ident}
    [:div.value
     [:div.anchors (links/render-links (remove :link/render-inline? my-links) ctx)]
     (let [widget (case (:layout ctx) :block code/code-block*
                                      :inline-block code/code-inline-block*
                                      :table code/code-inline-block*)
           props (assoc props :mode "markdown" :lineWrapping true)]
       [widget props (:value ctx) change!])               ; backwards args - props last
     (links/render-inline-links (filter :link/render-inline? my-links) ctx)]))
