(ns hypercrud.ui.attribute.edn
  (:require [clojure.set :as set]
            [contrib.datomic-tx :as tx]
            [contrib.reactive :as r]
            [contrib.ui :refer [edn-block edn-inline-block]]
            [hypercrud.browser.link :as link]
            [hypercrud.ui.control.link-controls :as link-controls]))


(defn edn-many [maybe-field props ctx]
  (let [valueType @(r/cursor (:hypercrud.browser/fat-attribute ctx) [:db/valueType :db/ident])
        value (-> (if (= valueType :db.type/ref)
                    (map :db/id @(:value ctx))
                    @(:value ctx))
                  set)
        change! (fn [user-val]
                  (let [user-val (set user-val)
                        rets (set/difference value user-val)
                        adds (set/difference user-val value)]
                    ((:user-with! ctx) (tx/edit-entity (-> ctx :cell-data deref :db/id)
                                                       (:hypercrud.browser/attribute ctx)
                                                       rets adds))))
        widget (case (:layout ctx) :block edn-block
                                   :inline-block edn-inline-block
                                   :table edn-inline-block)
        path [(:fe-pos ctx) (:hypercrud.browser/attribute ctx)]]
    [:div.value
     [:div.anchors (link-controls/anchors path true ctx link/options-processor)]
     [widget value change! props]
     (link-controls/iframes path true ctx link/options-processor)]))

(defn edn [maybe-field props ctx]
  (let [change! #((:user-with! ctx) (tx/update-entity-attr @(:cell-data ctx) @(:hypercrud.browser/fat-attribute ctx) %))
        widget (case (:layout ctx) :block edn-block
                                   :inline-block edn-inline-block
                                   :table edn-inline-block)
        path [(:fe-pos ctx) (:hypercrud.browser/attribute ctx)]]
    [:div.value
     [:div.anchors (link-controls/anchors path true ctx link/options-processor)]
     [widget @(:value ctx) change! props]
     (link-controls/iframes path true ctx link/options-processor)]))
