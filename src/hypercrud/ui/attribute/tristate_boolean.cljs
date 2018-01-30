(ns hypercrud.ui.attribute.tristate-boolean
  (:require [hypercrud.ui.control.link-controls :as links]
            [hypercrud.browser.link :as link]
            [hypercrud.ui.select :refer [select-boolean*]]))



(defn tristate-boolean [maybe-field props ctx]
  (let [my-links (link/links-lookup' (:links ctx) [(:fe-pos ctx) (-> ctx :attribute :db/ident)])]
    [:div.value
     [:div.editable-select {:key (:db/ident (:attribute ctx))}
      [:div.anchors (links/render-links (remove :link/render-inline? my-links) ctx)]
      (select-boolean* @(:value ctx) props ctx)]
     (links/render-inline-links (filter :link/render-inline? my-links) ctx)]))
