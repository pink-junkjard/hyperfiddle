(ns hypercrud.ui.attribute.checkbox
  (:require [hypercrud.ui.control.link-controls :as links]
            [hypercrud.browser.link :as link]
            [hypercrud.ui.control.checkbox :refer [checkbox*]]
            [hyperfiddle.foundation.actions :refer [with]]
            [hypercrud.client.tx :as tx]))


(defn checkbox [maybe-field props ctx]
  (let [my-links (link/links-lookup' (:links ctx) [(:fe-pos ctx) (-> ctx :attribute :db/ident)])]
    [:div.value
     [:div.editable-select {:key (:db/ident (:attribute ctx))}
      [:div.anchors (links/render-links (remove :link/render-inline? my-links) ctx)]
      (let [change! #((:user-with! ctx) (tx/update-entity-attr @(:cell-data ctx) (:attribute ctx) (not @(:value ctx))))]
        (checkbox* (:value ctx) change!))]
     (links/render-inline-links (filter :link/render-inline? my-links) ctx)]))
