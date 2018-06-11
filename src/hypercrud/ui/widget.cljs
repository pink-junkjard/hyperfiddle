(ns hypercrud.ui.widget
  (:refer-clojure :exclude [boolean keyword long])
  (:require [contrib.datomic-tx :as tx]
            [contrib.reactive :as r]
            [contrib.string :refer [empty->nil]]
            [contrib.ui.input :as input]
            [hypercrud.ui.select :refer [select*]]))


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

(defn ^:export boolean [value ctx props]
  [:div
   (let [props (update props :read-only #(or % (nil? @(r/cursor (:cell-data ctx) [:db/id]))))
         change! #((:user-with! ctx) (tx/update-entity-attr @(:cell-data ctx)
                                                            @(:hypercrud.browser/fat-attribute ctx)
                                                            (not value)))]
     (contrib.ui/checkbox value change! props))])

(defn id* [value ctx props]
  (let [props (update props :read-only #(or % (nil? @(r/cursor (:cell-data ctx) [:db/id]))))
        on-change! #((:user-with! ctx) (tx/update-entity-attr @(:cell-data ctx) @(:hypercrud.browser/fat-attribute ctx) %))]
    (input/id-input value on-change! props)))

(defn dbid [value ctx props]
  [:div (id* value ctx props)])

(defn select [value ctx props]
  [:div (select* value ctx props)])

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
