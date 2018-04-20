(ns hypercrud.ui.widget
  (:refer-clojure :exclude [boolean keyword long])
  (:require [contrib.datomic-tx :as tx]
            [contrib.reactive :as r]
            [hypercrud.browser.link :as link]
            [hypercrud.ui.control.link-controls :as link-controls]
            [hypercrud.ui.input :as input]

    ;user land (todo these should be in a core hc.ui namespace; widget is arbitrary)
            [hypercrud.ui.attribute.checkbox :refer [checkbox]]
            [hypercrud.ui.attribute.code]
            [hypercrud.ui.attribute.markdown-editor]
            [hypercrud.ui.attribute.tristate-boolean]
            [hypercrud.ui.radio]
            [hypercrud.ui.select :refer [select*]]
            [hypercrud.ui.textarea]))


(defn keyword [maybe-field props ctx]
  (let [path [(:fe-pos ctx) (:hypercrud.browser/attribute ctx)]]
    [:div.value
     [:div.anchors (link-controls/anchors path true ctx)]
     (let [on-change! #((:user-with! ctx) (tx/update-entity-attr @(:cell-data ctx) @(:hypercrud.browser/fat-attribute ctx) %))]
       [input/keyword-input* @(:value ctx) on-change! props])
     (link-controls/iframes path true ctx)]))

(defn string [maybe-field props ctx]
  (let [path [(:fe-pos ctx) (:hypercrud.browser/attribute ctx)]]
    [:div.value
     [:div.anchors (link-controls/anchors path true ctx)]
     (let [on-change! #((:user-with! ctx) (tx/update-entity-attr @(:cell-data ctx) @(:hypercrud.browser/fat-attribute ctx) %))]
       [input/input* @(:value ctx) on-change! props])
     (link-controls/iframes path true ctx)]))

(defn long [maybe-field props ctx]
  (let [path [(:fe-pos ctx) (:hypercrud.browser/attribute ctx)]]
    [:div.value
     [:div.anchors (link-controls/anchors path true ctx)]
     [input/validated-input
      @(:value ctx) #((:user-with! ctx) (tx/update-entity-attr @(:cell-data ctx) @(:hypercrud.browser/fat-attribute ctx) %))
      #(js/parseInt % 10) (fnil str "")
      #(or #_(= "" %) (integer? (js/parseInt % 10)))
      props]
     (link-controls/iframes path true ctx)]))

(def boolean checkbox)

(defn id* [props ctx]
  (let [on-change! #((:user-with! ctx) (tx/update-entity-attr @(:cell-data ctx) @(:hypercrud.browser/fat-attribute ctx) %))]
    (input/id-input @(:value ctx) on-change! props)))

; this can be used sometimes, on the entity page, but not the query page
(defn ref [maybe-field props ctx]
  (let [path [(:fe-pos ctx) (:hypercrud.browser/attribute ctx)]]
    [:div.value
     [:div.editable-select
      [:div.anchors (link-controls/anchors path true ctx link/options-processor)] ;todo can this be lifted out of editable-select?
      (if-let [options-link @(r/track link/options-link path ctx)]
        [:div.select                                        ; helps the weird link float left css thing
         (select* options-link props ctx)]
        (id* props ctx))]
     (link-controls/iframes path true ctx link/options-processor)]))

(defn ref-component [maybe-field props ctx]
  (let [path [(:fe-pos ctx) (:hypercrud.browser/attribute ctx)]]
    (assert (not @(r/track link/options-link path ctx)) "ref-components don't have options; todo handle gracefully")
    #_(assert (> (count (filter :link/render-inline? my-links)) 0))
    #_(ref maybe-field my-links props ctx)
    [:div.value
     [:div.anchors (link-controls/anchors path true ctx)]
     #_[:pre (pr-str @(:value ctx))]
     (link-controls/iframes path true ctx)]))

(defn ref-many-table [maybe-field props ctx]
  (let [path [(:fe-pos ctx) (:hypercrud.browser/attribute ctx)]]
    (assert (not @(r/track link/options-link path ctx)) "ref-component-many don't have options; todo handle gracefully")
    [:div.value
     #_[:pre (pr-str maybe-field)]
     [:div.anchors (link-controls/anchors path true ctx)]
     (link-controls/iframes path true ctx)]))

(defn ref-many-component-table [maybe-field props ctx]
  (let [path [(:fe-pos ctx) (:hypercrud.browser/attribute ctx)]]
    [:div.value
     [:div.anchors (link-controls/anchors path true ctx)]
     (link-controls/iframes path true ctx)]))

(defn text [maybe-field props ctx]
  (let [path [(:fe-pos ctx) (:hypercrud.browser/attribute ctx)]]
    [:div.value
     [:div.anchors (link-controls/anchors path true ctx)]
     [:span.text
      (case @(r/cursor (:hypercrud.browser/fat-attribute ctx) [:db/cardinality :db/ident])
        :db.cardinality/many (map pr-str @(:value ctx))
        (pr-str @(:value ctx)))]
     (link-controls/iframes path true ctx)]))
