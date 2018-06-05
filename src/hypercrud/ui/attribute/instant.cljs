(ns hypercrud.ui.attribute.instant
  (:require [contrib.datomic-tx :as tx]
            [contrib.reactive :as r]
            [contrib.ui.recom-date :refer [recom-date]]
            [hypercrud.ui.control.link-controls :as link-controls]))


(defn ^:export instant [value ctx props]
  (let [path [(:fe-pos ctx) (:hypercrud.browser/attribute ctx)]]
    [:div.hyperfiddle-ui-instant
     [:div.anchors (link-controls/anchors path true ctx)]
     (let [props (update props :read-only #(or % (nil? @(r/cursor (:cell-data ctx) [:db/id]))))
           change! #((:user-with! ctx) (tx/update-entity-attr @(:cell-data ctx) @(:hypercrud.browser/fat-attribute ctx) %))
           widget (case (:hyperfiddle.ui/layout ctx)
                    :hyperfiddle.ui.layout/block recom-date
                    :hyperfiddle.ui.layout/inline-block recom-date
                    :hyperfiddle.ui.layout/table recom-date)]
       [widget value change! props])
     (link-controls/iframes path true ctx)]))
