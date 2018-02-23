(ns hypercrud.ui.attribute.checkbox
  (:require [hypercrud.client.tx :as tx]
            [hypercrud.ui.control.checkbox :refer [checkbox*]]
            [hypercrud.ui.control.link-controls :as link-controls]))


(defn checkbox [maybe-field props ctx]
  (let [path [(:fe-pos ctx) (-> ctx :attribute :db/ident)]]
    [:div.value
     [:div.editable-select {:key (:db/ident (:attribute ctx))}
      [:div.anchors (link-controls/render-nav-cmps path true ctx)]
      (let [change! #((:user-with! ctx) (tx/update-entity-attr @(:cell-data ctx) (:attribute ctx) (not @(:value ctx))))]
        (checkbox* (:value ctx) change!))]
     (link-controls/render-inline-links path true ctx)]))
