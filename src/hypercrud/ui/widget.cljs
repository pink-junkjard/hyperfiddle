(ns hypercrud.ui.widget
  (:refer-clojure :exclude [keyword long boolean])
  (:require [clojure.set :as set]
            [hypercrud.browser.link :as link]
            [hypercrud.browser.base :as base]
            [hypercrud.browser.core :as browser]
            [hypercrud.client.tx :as tx]
            [hypercrud.ui.control.link-controls :as links]
            [hypercrud.ui.input :as input]
            [hypercrud.ui.radio]                            ; used in user renderers
            [hypercrud.ui.select :refer [select* select-boolean*]]
            [hypercrud.ui.textarea :refer [textarea*]]

    ;compat
            [hypercrud.ui.attribute.code]
            [hypercrud.ui.attribute.markdown-editor]
            [hypercrud.ui.attribute.edn]))


(def ^:export code hypercrud.ui.attribute.code/code)
(def ^:export markdown hypercrud.ui.attribute.markdown-editor/markdown-editor)
(def ^:export edn hypercrud.ui.attribute.edn/edn)
(def ^:export edn-many hypercrud.ui.attribute.edn/edn-many)

(defn keyword [maybe-field props ctx]
  (let [my-links (->> (link/links-lookup' (:links ctx) [(:fe-pos ctx) (-> ctx :attribute :db/ident)])
                      (filter :link/dependent?)) #_"this also has to happen every other thing, problem is that :options need to show up here for ref even if not repeating"]
    [:div.value
     [:div.anchors (links/render-links (remove :link/render-inline? my-links) ctx)]
     (let [on-change! #((:user-with! ctx) (tx/update-entity-attr @(:cell-data ctx) (:attribute ctx) %))]
       [input/keyword-input* @(:value ctx) on-change! props])
     (links/render-inline-links (filter :link/render-inline? my-links) ctx)]))

(defn string [maybe-field props ctx]
  (let [my-links (link/links-lookup' (:links ctx) [(:fe-pos ctx) (-> ctx :attribute :db/ident)])]
    [:div.value
     [:div.anchors (links/render-links (remove :link/render-inline? my-links) ctx)]
     (let [on-change! #((:user-with! ctx) (tx/update-entity-attr @(:cell-data ctx) (:attribute ctx) %))]
       [input/input* @(:value ctx) on-change! props])
     (links/render-inline-links (filter :link/render-inline? my-links) ctx)]))

(defn long [maybe-field props ctx]
  (let [my-links (link/links-lookup' (:links ctx) [(:fe-pos ctx) (-> ctx :attribute :db/ident)])]
    [:div.value
     [:div.anchors (links/render-links (remove :link/render-inline? my-links) ctx)]
     [input/validated-input
      @(:value ctx) #((:user-with! ctx) (tx/update-entity-attr @(:cell-data ctx) (:attribute ctx) %))
      #(js/parseInt % 10) pr-str
      #(or (integer? (js/parseInt % 10)) (= "nil" %))
      props]
     (links/render-inline-links (filter :link/render-inline? my-links) ctx)]))

(defn boolean [maybe-field props ctx]
  (let [my-links (link/links-lookup' (:links ctx) [(:fe-pos ctx) (-> ctx :attribute :db/ident)])]
    [:div.value
     [:div.editable-select {:key (:db/ident (:attribute ctx))}
      [:div.anchors (links/render-links (remove :link/render-inline? my-links) ctx)]
      (select-boolean* @(:value ctx) props ctx)]
     (links/render-inline-links (filter :link/render-inline? my-links) ctx)]))

(defn id* [props ctx]
  (let [on-change! #((:user-with! ctx) (tx/update-entity-attr @(:cell-data ctx) (:attribute ctx) %))]
    (input/id-input @(:value ctx) on-change! props)))

; this can be used sometimes, on the entity page, but not the query page
(defn ref [maybe-field props ctx]
  (let [my-links (link/links-lookup' (:links ctx) [(:fe-pos ctx) (-> ctx :attribute :db/ident)])
        [my-links options-link] (link/process-option-links my-links ctx)
        my-links (->> my-links (filter :link/dependent?))]
    [:div.value
     [:div.editable-select
      [:div.anchors (links/render-links (remove :link/render-inline? my-links) ctx)] ;todo can this be lifted out of editable-select?
      [:div.select                                          ; helps the weird link float left css thing
       (if options-link
         (select* options-link props ctx)
         (id* props ctx))]]
     (links/render-inline-links (filter :link/render-inline? my-links) ctx)]))

(defn ref-component [maybe-field props ctx]
  (let [my-links (link/links-lookup' (:links ctx) [(:fe-pos ctx) (-> ctx :attribute :db/ident)])
        [my-links options-link] (link/process-option-links my-links ctx)
        my-links (->> my-links (filter :link/dependent?))]
    (assert (not options-link) "ref-components don't have options; todo handle gracefully")
    #_(assert (> (count (filter :link/render-inline? my-links)) 0))
    #_(ref maybe-field my-links props ctx)
    [:div.value
     [:div.anchors (links/render-links (remove :link/render-inline? my-links) ctx)]
     #_[:pre (pr-str @(:value ctx))]
     (links/render-inline-links (filter :link/render-inline? my-links) ctx)]))

(defn ref-many-table [maybe-field props ctx]
  (let [my-links (link/links-lookup' (:links ctx) [(:fe-pos ctx) (-> ctx :attribute :db/ident)])
        [my-links options-link] (link/process-option-links my-links ctx)
        my-links (->> my-links (filter :link/dependent?))]
    (assert (not options-link) "ref-component-many don't have options; todo handle gracefully")
    [:div.value
     #_[:pre (pr-str maybe-field)]
     [:div.anchors (links/render-links (remove :link/render-inline? my-links) ctx)]
     (links/render-inline-links (filter :link/render-inline? my-links) ctx)]))

(defn ref-many-component-table [maybe-field props ctx]
  (let [my-links (link/links-lookup' (:links ctx) [(:fe-pos ctx) (-> ctx :attribute :db/ident)])]
    [:div.value
     [:div.anchors (links/render-links (remove :link/render-inline? my-links) ctx)]
     (links/render-inline-links (filter :link/render-inline? my-links) ctx)]))

(defn multi-select-ref [maybe-field props ctx]
  (assert false "todo")
  #_(let [add-item! #((:user-with! ctx) (tx/edit-entity (:db/id @(:cell-data ctx)) (:attribute ctx) [] [nil]))]
      (multi-select* multi-select-markup add-item! maybe-field props ctx))) ;add-item! is: add nil to set

;(defn multi-select-ref-component [maybe-field props ctx]
;  (let [add-item! #((:user-swap! ctx) {:tx (tx/edit-entity (:db/id @(:cell-data ctx)) (:attribute ctx) [] [(temp-id!)])})]
;    [multi-select* multi-select-markup add-item! maybe-field props ctx])) ;add new entity to set

(defn text [maybe-field props ctx]
  (let [my-links (link/links-lookup' (:links ctx) [(:fe-pos ctx) (-> ctx :attribute :db/ident)])]
    [:div.value
     [:div.anchors (links/render-links (remove :link/render-inline? my-links) ctx)]
     [:span.text
      (case (-> (:attribute ctx) :db/cardinality :db/ident)
        :db.cardinality/many (map pr-str @(:value ctx))
        (pr-str @(:value ctx)))]
     (links/render-inline-links (filter :link/render-inline? my-links) ctx)]))
