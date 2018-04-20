(ns hypercrud.ui.attribute.instant
  (:require [contrib.datomic-tx :as tx]
            [hypercrud.ui.control.instant :as instant]
            [hypercrud.ui.control.link-controls :as link-controls]))


(defn instant [maybe-field props ctx]
  (let [path [(:fe-pos ctx) (:hypercrud.browser/attribute ctx)]]
    [:div.value.hyperfiddle-ui-instant
     [:div.anchors (link-controls/anchors path true ctx)]
     (let [change! #((:user-with! ctx) (tx/update-entity-attr @(:cell-data ctx) @(:hypercrud.browser/fat-attribute ctx) %))
           widget (case (:layout ctx) :block instant/recom-date*
                                      :inline-block instant/recom-date*
                                      :table instant/recom-date*)]
       [widget @(:value ctx) change! props])
     (link-controls/iframes path true ctx)]))
