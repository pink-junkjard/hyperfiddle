(ns hypercrud.ui.attribute.instant
  (:require [contrib.datomic-tx :as tx]
            [contrib.reactive :as r]
            [contrib.ui.recom-date :refer [recom-date]]))


(defn ^:export instant [value ctx props]
  [:div
   (let [props (update props :read-only #(or % (nil? @(r/cursor (:cell-data ctx) [:db/id]))))
         change! #((:user-with! ctx) (tx/update-entity-attr @(:cell-data ctx) @(:hypercrud.browser/fat-attribute ctx) %))
         widget (case (:hyperfiddle.ui/layout ctx :hyperfiddle.ui.layout/block)
                  :hyperfiddle.ui.layout/block recom-date
                  :hyperfiddle.ui.layout/inline-block recom-date
                  :hyperfiddle.ui.layout/table recom-date)]
     [widget value change! props])])
