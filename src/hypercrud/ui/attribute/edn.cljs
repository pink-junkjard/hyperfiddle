(ns hypercrud.ui.attribute.edn
  (:require [clojure.set :as set]
            [hypercrud.client.tx :as tx]
            [hypercrud.browser.link :as link]
            [hypercrud.ui.control.link-controls :as link-controls]
            [hypercrud.ui.control.edn :refer [edn-block* edn-inline-block*]]))


(defn edn-many [maybe-field links props ctx]
  (let [valueType (-> ctx :attribute :db/valueType :db/ident)
        value (-> (if (= valueType :db.type/ref)
                    (map :db/id (:value ctx))
                    (:value ctx))
                  set)
        change! (fn [user-val]
                  (let [user-val (set user-val)
                        rets (set/difference value user-val)
                        adds (set/difference user-val value)]
                    ((:user-with! ctx) (tx/edit-entity (-> ctx :cell-data :db/id)
                                                       (-> ctx :attribute :db/ident)
                                                       rets adds))))
        widget (case (:layout ctx) :block edn-block*
                                   :inline-block edn-inline-block*
                                   :table edn-inline-block*)
        my-links (link/links-lookup' links [(:fe-pos ctx) (-> ctx :attribute :db/ident)])
        [my-links options-link] (link/process-option-links my-links ctx)
        my-links (->> my-links (filter :link/dependent?))]
    [:div.value
     [:div.anchors (link-controls/render-links (remove :link/render-inline? my-links) ctx)]
     [widget value change! props]
     (link-controls/render-inline-links (filter :link/render-inline? my-links) ctx)]))

(defn edn [maybe-field links props ctx]
  (let [valueType (-> ctx :attribute :db/valueType :db/ident)
        value (if (= valueType :db.type/ref) (:db/id (:value ctx)) (:value ctx))
        change! #((:user-with! ctx) (tx/update-entity-attr (:cell-data ctx) (:attribute ctx) %))
        widget (case (:layout ctx) :block edn-block*
                                   :inline-block edn-inline-block*
                                   :table edn-inline-block*)
        my-links (link/links-lookup' links [(:fe-pos ctx) (-> ctx :attribute :db/ident)])
        [my-links options-link] (link/process-option-links my-links ctx)
        my-links (->> my-links (filter :link/dependent?))]
    [:div.value
     [:div.anchors (link-controls/render-links (remove :link/render-inline? my-links) ctx)]
     [widget (:value ctx) change! props]
     (link-controls/render-inline-links (filter :link/render-inline? my-links) ctx)]))
