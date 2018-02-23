(ns hypercrud.ui.attribute.edn
  (:require [clojure.set :as set]
            [hypercrud.browser.link :as link]
            [hypercrud.client.tx :as tx]
            [hypercrud.ui.control.edn :refer [edn-block* edn-inline-block*]]
            [hypercrud.ui.control.link-controls :as link-controls]))


(defn edn-many [maybe-field props ctx]
  (let [valueType (-> ctx :attribute :db/valueType :db/ident)
        value (-> (if (= valueType :db.type/ref)
                    (map :db/id @(:value ctx))
                    @(:value ctx))
                  set)
        change! (fn [user-val]
                  (let [user-val (set user-val)
                        rets (set/difference value user-val)
                        adds (set/difference user-val value)]
                    ((:user-with! ctx) (tx/edit-entity (-> ctx :cell-data deref :db/id)
                                                       (-> ctx :attribute :db/ident)
                                                       rets adds))))
        widget (case (:layout ctx) :block edn-block*
                                   :inline-block edn-inline-block*
                                   :table edn-inline-block*)
        path [(:fe-pos ctx) (-> ctx :attribute :db/ident)]]
    [:div.value
     [:div.anchors (link-controls/render-nav-cmps path true ctx link/options-processor)]
     [widget value change! props]
     (link-controls/render-inline-links path true ctx link/options-processor)]))

(defn edn [maybe-field props ctx]
  (let [valueType (-> ctx :attribute :db/valueType :db/ident)
        value (if (= valueType :db.type/ref) (:db/id @(:value ctx)) @(:value ctx))
        change! #((:user-with! ctx) (tx/update-entity-attr @(:cell-data ctx) (:attribute ctx) %))
        widget (case (:layout ctx) :block edn-block*
                                   :inline-block edn-inline-block*
                                   :table edn-inline-block*)
        path [(:fe-pos ctx) (-> ctx :attribute :db/ident)]]
    [:div.value
     [:div.anchors (link-controls/render-nav-cmps path true ctx link/options-processor)]
     [widget @(:value ctx) change! props]
     (link-controls/render-inline-links path true ctx link/options-processor)]))
