(ns hypercrud.ui.widget
  (:refer-clojure :exclude [boolean keyword long])
  (:require [contrib.datomic-tx :as tx]
            [contrib.reactive :as r]
            [contrib.string :refer [empty->nil]]
            [hypercrud.browser.link :as link]
            [hypercrud.ui.control.link-controls :as link-controls]
            [contrib.ui.input :as input]
            [hypercrud.ui.attribute.checkbox :refer [checkbox]]
            [hypercrud.ui.select :refer [select*]]
            ))


(defn keyword [value ctx props]
  (let [path [(:fe-pos ctx) (:hypercrud.browser/attribute ctx)]]
    [:div
     [:div.anchors (link-controls/anchors path true ctx nil)]
     (let [props (update props :read-only #(or % (nil? @(r/cursor (:cell-data ctx) [:db/id]))))
           on-change! #((:user-with! ctx) (tx/update-entity-attr @(:cell-data ctx) @(:hypercrud.browser/fat-attribute ctx) %))]
       [input/keyword-input* value on-change! props])
     (link-controls/iframes path true ctx)]))

(defn string [value ctx props]
  (let [path [(:fe-pos ctx) (:hypercrud.browser/attribute ctx)]]
    [:div
     [:div.anchors (link-controls/anchors path true ctx nil)]
     (let [props (update props :read-only #(or % (nil? @(r/cursor (:cell-data ctx) [:db/id]))))
           on-change! #((:user-with! ctx) (tx/update-entity-attr @(:cell-data ctx) @(:hypercrud.browser/fat-attribute ctx) (empty->nil %)))]
       [input/input* value on-change! props])
     (link-controls/iframes path true ctx)]))

(defn long [value ctx props]
  (let [path [(:fe-pos ctx) (:hypercrud.browser/attribute ctx)]
        props (update props :read-only #(or % (nil? @(r/cursor (:cell-data ctx) [:db/id]))))
        on-change! #((:user-with! ctx) (tx/update-entity-attr @(:cell-data ctx) @(:hypercrud.browser/fat-attribute ctx) %))]
    [:div
     [:div.anchors (link-controls/anchors path true ctx nil)]
     [input/validated-input
      value on-change!
      #(js/parseInt % 10) (fnil str "")
      #(or #_(= "" %) (integer? (js/parseInt % 10)))
      props]
     (link-controls/iframes path true ctx)]))

(def boolean checkbox)

(defn id* [value ctx props]
  (let [props (update props :read-only #(or % (nil? @(r/cursor (:cell-data ctx) [:db/id]))))
        on-change! #((:user-with! ctx) (tx/update-entity-attr @(:cell-data ctx) @(:hypercrud.browser/fat-attribute ctx) %))]
    (input/id-input value on-change! props)))

; this can be used sometimes, on the entity page, but not the query page
(defn ref [value ctx props]
  (let [path [(:fe-pos ctx) (:hypercrud.browser/attribute ctx)]]
    [:div
     (link-controls/anchors path true ctx link/options-processor props) ;todo can this be lifted out of editable-select?
     (if-let [options-link @(r/track link/options-link path ctx)]
       (select* options-link ctx props)
       (id* value ctx props))
     (link-controls/iframes path true ctx link/options-processor)]))

(defn ref-component [value ctx props]
  (let [path [(:fe-pos ctx) (:hypercrud.browser/attribute ctx)]]
    (assert (not @(r/track link/options-link path ctx)) "ref-components don't have options; todo handle gracefully")
    #_(assert (> (count (filter :link/render-inline? my-links)) 0))
    #_(ref my-links props ctx)
    [:div
     [:div.anchors (link-controls/anchors path true ctx nil)]
     #_[:pre (pr-str value)]
     (link-controls/iframes path true ctx)]))

(defn ref-many-table [value ctx props]
  (let [path [(:fe-pos ctx) (:hypercrud.browser/attribute ctx)]]
    (assert (not @(r/track link/options-link path ctx)) "ref-component-many don't have options; todo handle gracefully")
    [:div
     [:div.anchors (link-controls/anchors path true ctx nil)]
     (link-controls/iframes path true ctx)]))

(defn ref-many-component-table [value ctx props]
  (let [path [(:fe-pos ctx) (:hypercrud.browser/attribute ctx)]]
    [:div
     [:div.anchors (link-controls/anchors path true ctx nil)]
     (link-controls/iframes path true ctx)]))

(defn text [value ctx props]
  (let [path [(:fe-pos ctx) (:hypercrud.browser/attribute ctx)]]
    [:div
     [:div.anchors (link-controls/anchors path true ctx nil)]
     [:span.text
      (case @(r/cursor (:hypercrud.browser/fat-attribute ctx) [:db/cardinality :db/ident])
        :db.cardinality/many (map pr-str value)
        (pr-str value))]
     (link-controls/iframes path true ctx)]))
