(ns hypercrud.ui.attribute.code
  (:require [contrib.datomic-tx :as tx]
            [contrib.string :refer [empty->nil]]
            [hypercrud.ui.control.code :as code]
            [hypercrud.ui.control.link-controls :as link-controls]))


(defn ^:export code [field props ctx]
  (let [path [(:fe-pos ctx) (:hypercrud.browser/attribute ctx)]
        change! (fn [%]
                  (let [tx (tx/update-entity-attr @(:cell-data ctx) @(:hypercrud.browser/fat-attribute ctx) (empty->nil %))]
                    ((:user-with! ctx) tx)))]
    [:div.value
     [:div.anchors (link-controls/anchors path true ctx)]
     (let [control (case (:layout ctx :block)
                     :block code/code-block*
                     :inline-block code/code-inline-block*
                     :table code/code-inline-block*)]
       [control props @(:value ctx) change!])               ; backwards args - props last
     (link-controls/iframes path true ctx)]))
