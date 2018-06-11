(ns hypercrud.ui.attribute.checkbox
  (:require [contrib.datomic-tx :as tx]
            [contrib.reactive :as r]
            [contrib.ui]))


(defn ^:export checkbox [value ctx props]
  [:div
   (let [props (update props :read-only #(or % (nil? @(r/cursor (:cell-data ctx) [:db/id]))))
         change! #((:user-with! ctx) (tx/update-entity-attr @(:cell-data ctx)
                                                            @(:hypercrud.browser/fat-attribute ctx)
                                                            (not value)))]
     (contrib.ui/checkbox value change! props))])
