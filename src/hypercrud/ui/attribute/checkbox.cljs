(ns hypercrud.ui.attribute.checkbox
  (:require [contrib.datomic-tx :as tx]
            [contrib.reactive :as r]
            [contrib.ui]
            [hypercrud.ui.control.link-controls :as link-controls]))


(defn ^:export checkbox [value ctx props]
  (let [path [(:fe-pos ctx) (:hypercrud.browser/attribute ctx)]]
    [:div
     [:div.editable-select {:key (:hypercrud.browser/attribute ctx)}
      [:div.anchors (link-controls/anchors path true ctx nil)]
      (let [props (update props :read-only #(or % (nil? @(r/cursor (:cell-data ctx) [:db/id]))))
            change! #((:user-with! ctx) (tx/update-entity-attr @(:cell-data ctx)
                                                               @(:hypercrud.browser/fat-attribute ctx)
                                                               (not value)))]
        (contrib.ui/checkbox value change! props))]
     (link-controls/iframes path true ctx)]))
