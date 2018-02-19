(ns hypercrud.ui.attribute.instant
  (:require [hypercrud.browser.link :as link]
            [hypercrud.client.tx :as tx]
            [hypercrud.ui.control.edn :as edn]
            [hypercrud.ui.control.instant :as instant]
            [hypercrud.ui.control.link-controls :as link-controls]))


(defn instant [maybe-field props ctx]
  (let [my-links (link/links-lookup' (:links ctx) [(:fe-pos ctx) (-> ctx :attribute :db/ident)])]
    [:div.value.hyperfiddle-ui-instant
     [:div.anchors (link-controls/render-links (remove :link/render-inline? my-links) ctx)]
     (let [change! #((:user-with! ctx) (tx/update-entity-attr @(:cell-data ctx) (:attribute ctx) %))
           widget (case (:layout ctx) :block instant/recom-date*
                                      :inline-block instant/recom-date*
                                      :table instant/recom-date*)]
       [widget @(:value ctx) change! props])
     (link-controls/render-inline-links (filter :link/render-inline? my-links) ctx)]))