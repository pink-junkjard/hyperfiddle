(ns hypercrud.ui.attribute.markdown-editor
  (:require [hypercrud.client.tx :as tx]
            [hypercrud.ui.control.code :as code]
            [hypercrud.ui.control.link-controls :as link-controls]))


(defn ^:export markdown-editor [field props ctx]
  (let [path [(:fe-pos ctx) (-> ctx :attribute :db/ident)]
        change! #((:user-with! ctx) (tx/update-entity-attr @(:cell-data ctx) (:attribute ctx) %))]
    ;^{:key ident}
    [:div.value
     [:div.anchors (link-controls/render-nav-cmps path true ctx)]
     (let [widget (case (:layout ctx) :block code/code-block*
                                      :inline-block code/code-inline-block*
                                      :table code/code-inline-block*)
           props (assoc props :mode "markdown" :lineWrapping true)]
       [widget props @(:value ctx) change!])                 ; backwards args - props last
     (link-controls/render-inline-links path true ctx)]))
