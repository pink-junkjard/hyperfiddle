(ns hypercrud.ui.attribute.checkbox
  (:require [contrib.datomic-tx :as tx]
            [hypercrud.ui.control.checkbox :refer [checkbox*]]
            [hypercrud.ui.control.link-controls :as link-controls]))


(defn checkbox [maybe-field props ctx]
  (let [path [(:fe-pos ctx) (:hypercrud.browser/attribute ctx)]]
    [:div.value
     [:div.editable-select {:key (:hypercrud.browser/attribute ctx)}
      [:div.anchors (link-controls/render-nav-cmps path true ctx)]
      (let [change! #((:user-with! ctx) (tx/update-entity-attr @(:cell-data ctx)
                                                               @(:hypercrud.browser/fat-attribute ctx)
                                                               (not @(:value ctx))))]
        (checkbox* (:value ctx) change!))]
     (link-controls/render-inline-links path true ctx)]))
