(ns hypercrud.ui.widget
  (:refer-clojure :exclude [boolean keyword long])
  (:require [contrib.datomic-tx :as tx]
            [contrib.reactive :as r]
            [contrib.string :refer [empty->nil]]
            [hypercrud.browser.link :as link]
            [contrib.ui.input :as input]
            [hypercrud.ui.attribute.checkbox :refer [checkbox]]
            [hypercrud.ui.select :refer [select*]]
            ))


(defn keyword [value ctx props]
  [:div
   (let [props (update props :read-only #(or % (nil? @(r/cursor (:cell-data ctx) [:db/id]))))
         on-change! #((:user-with! ctx) (tx/update-entity-attr @(:cell-data ctx) @(:hypercrud.browser/fat-attribute ctx) %))]
     [input/keyword-input* value on-change! props])])

(defn string [value ctx props]
  [:div
   (let [props (update props :read-only #(or % (nil? @(r/cursor (:cell-data ctx) [:db/id]))))
         on-change! #((:user-with! ctx) (tx/update-entity-attr @(:cell-data ctx) @(:hypercrud.browser/fat-attribute ctx) (empty->nil %)))]
     [input/input* value on-change! props])])

(defn long [value ctx props]
  (let [props (update props :read-only #(or % (nil? @(r/cursor (:cell-data ctx) [:db/id]))))
        on-change! #((:user-with! ctx) (tx/update-entity-attr @(:cell-data ctx) @(:hypercrud.browser/fat-attribute ctx) %))]
    [:div
     [input/validated-input
      value on-change!
      #(js/parseInt % 10) (fnil str "")
      #(or #_(= "" %) (integer? (js/parseInt % 10)))
      props]]))

(def boolean checkbox)

(defn id* [value ctx props]
  (let [props (update props :read-only #(or % (nil? @(r/cursor (:cell-data ctx) [:db/id]))))
        on-change! #((:user-with! ctx) (tx/update-entity-attr @(:cell-data ctx) @(:hypercrud.browser/fat-attribute ctx) %))]
    (input/id-input value on-change! props)))

(defn dbid [value ctx props]
  [:div (id* value ctx props)])

(defn select [value ctx props]
  [:div (select* value ctx props)])

(defn ref-component [value ctx props]
  (assert (not @(r/track link/options-link ctx)) "ref-components don't have options; todo handle gracefully")
  #_(assert (> (count (filter :link/render-inline? my-links)) 0))
  #_(select my-links props ctx)
  [:div])

(defn ref-many-table [value ctx props]
  (assert (not @(r/track link/options-link ctx)) "ref-component-many don't have options; todo handle gracefully")
  [:div])

(defn text [value ctx props]
  [:div
   [:span
    (case @(r/cursor (:hypercrud.browser/fat-attribute ctx) [:db/cardinality :db/ident])
      :db.cardinality/many (map pr-str value)
      (pr-str value))]])

;/*.value span.text {*/
;    /*margin: 3px 0;*/
;/*min-height: 15px;*/
;/*display: inline-block;*/
;/*line-height: normal;*/
;/*vertical-align: top;*/
;/*padding-left: 5px;*/
;/*}*/
