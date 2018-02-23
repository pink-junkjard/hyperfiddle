(ns hypercrud.ui.attribute.instant
  (:require [hypercrud.client.tx :as tx]
            [hypercrud.ui.control.instant :as instant]
            [hypercrud.ui.control.link-controls :as link-controls]))


(defn instant [maybe-field props ctx]
  (let [path [(:fe-pos ctx) (-> ctx :attribute :db/ident)]]
    [:div.value.hyperfiddle-ui-instant
     [:div.anchors (link-controls/render-nav-cmps path true ctx)]
     (let [change! #((:user-with! ctx) (tx/update-entity-attr @(:cell-data ctx) (:attribute ctx) %))
           widget (case (:layout ctx) :block instant/recom-date*
                                      :inline-block instant/recom-date*
                                      :table instant/recom-date*)]
       [widget @(:value ctx) change! props])
     (link-controls/render-inline-links path true ctx)]))
