(ns hypercrud.ui.widget
  (:refer-clojure :exclude [keyword long boolean])
  (:require [clojure.set :as set]
            [hypercrud.browser.anchor :as link]
            [hypercrud.browser.base :as base]
            [hypercrud.browser.core :as browser]
            [hypercrud.client.tx :as tx]
            [hypercrud.ui.code-editor :as code-editor]
            [hypercrud.ui.edn :refer [edn-block edn-inline-block]]
            [hypercrud.ui.input :as input]
            [hypercrud.ui.instant :refer [date* iso8601-string*]]
            [hypercrud.ui.radio]                            ; used in user renderers
            [hypercrud.ui.select :refer [select* select-boolean*]]
            [hypercrud.ui.textarea :refer [textarea*]]))


(defn render-link [link ctx]
  (let [prompt (or (:anchor/prompt link)                    ; ???
                   (:link/rel link)
                   "_")]
    [(:navigate-cmp ctx) (link/build-link-props link ctx) prompt]))

(defn render-links
  ([link-ctx-pairs]
   (->> link-ctx-pairs
        ; Don't filter hidden links; because they could be broken or invalid and need to draw error.
        (map (fn [[link ctx]]
               ^{:key (hash (:db/id link))}
               [render-link link ctx]))
        doall))
  ([links ctx]
   (render-links (map vector links (repeat ctx)))))

(defn render-inline-links
  ([links ctx]
   (render-inline-links (map vector links (repeat ctx))))
  ([link-ctx-pairs]
   (->> link-ctx-pairs
        ; Don't filter hidden links; because they could be broken or invalid and need to draw error.
        (map (fn [[link ctx]]
               ; don't test link validity, we need to render the failure. If this is a dependent link, use visibility predicate to hide the error.
               [:div {:key (hash (:db/id link))}            ; extra div bc had trouble getting keys to work
                ; NOTE: this ctx logic and structure is the same as the inline branch of browser-request/recurse-request
                [browser/ui link (update ctx :debug #(str % ">inline-link[" (:db/id link) ":" (or (:link/rel link) (:anchor/prompt link)) "]"))]]))
        (remove nil?)
        (doall))))

(defn keyword [maybe-field links props ctx]
  (let [my-links (->> (link/links-lookup' links [(:fe-pos ctx) (-> ctx :attribute :db/ident)])
                      (filter :link/dependent?)) #_"this also has to happen every other thing, problem is that :options need to show up here for ref even if not repeating"]
    [:div.value
     [:div.anchors (render-links (remove :link/render-inline? my-links) ctx)]
     (let [on-change! #((:user-with! ctx) (tx/update-entity-attr (:cell-data ctx) (:attribute ctx) %))]
       [input/keyword-input* (:value ctx) on-change! props])
     (render-inline-links (filter :link/render-inline? my-links) ctx)]))

(defn string [maybe-field links props ctx]
  (let [my-links (link/links-lookup' links [(:fe-pos ctx) (-> ctx :attribute :db/ident)])]
    [:div.value
     [:div.anchors (render-links (remove :link/render-inline? my-links) ctx)]
     (let [on-change! #((:user-with! ctx) (tx/update-entity-attr (:cell-data ctx) (:attribute ctx) %))]
       [input/input* (:value ctx) on-change! props])
     (render-inline-links (filter :link/render-inline? my-links) ctx)]))

(defn long [maybe-field links props ctx]
  (let [my-links (link/links-lookup' links [(:fe-pos ctx) (-> ctx :attribute :db/ident)])]
    [:div.value
     [:div.anchors (render-links (remove :link/render-inline? my-links) ctx)]
     [input/validated-input
      (:value ctx) #((:user-with! ctx) (tx/update-entity-attr (:cell-data ctx) (:attribute ctx) %))
      #(js/parseInt % 10) pr-str
      #(or (integer? (js/parseInt % 10)) (= "nil" %))
      props]
     (render-inline-links (filter :link/render-inline? my-links) ctx)]))

(defn boolean [maybe-field links props ctx]
  (let [my-links (link/links-lookup' links [(:fe-pos ctx) (-> ctx :attribute :db/ident)])]
    [:div.value
     [:div.editable-select {:key (:db/ident (:attribute ctx))}
      [:div.anchors (render-links (remove :link/render-inline? my-links) ctx)]
      (select-boolean* (:value ctx) props ctx)]
     (render-inline-links (filter :link/render-inline? my-links) ctx)]))

(defn id* [props ctx]
  (let [on-change! #((:user-with! ctx) (tx/update-entity-attr (:cell-data ctx) (:attribute ctx) %))]
    (input/id-input (:value ctx) on-change! props)))

; this can be used sometimes, on the entity page, but not the query page
(defn ref [maybe-field links props ctx]
  (let [my-links (link/links-lookup' links [(:fe-pos ctx) (-> ctx :attribute :db/ident)])
        [my-links options-link] (link/process-option-links my-links ctx)
        my-links (->> my-links (filter :link/dependent?))]
    [:div.value
     [:div.editable-select
      [:div.anchors (render-links (remove :link/render-inline? my-links) ctx)] ;todo can this be lifted out of editable-select?
      [:div.select                                          ; helps the weird link float left css thing
       (if options-link
         (select* (:value ctx) options-link props ctx)
         (id* props ctx))]]
     (render-inline-links (filter :link/render-inline? my-links) ctx)]))

(defn ref-component [maybe-field links props ctx]
  (let [my-links (link/links-lookup' links [(:fe-pos ctx) (-> ctx :attribute :db/ident)])
        [my-links options-link] (link/process-option-links my-links ctx)
        my-links (->> my-links (filter :link/dependent?))]
    (assert (not options-link) "ref-components don't have options; todo handle gracefully")
    #_(assert (> (count (filter :link/render-inline? my-links)) 0))
    #_(ref maybe-field my-links props ctx)
    [:div.value
     [:div.anchors (render-links (remove :link/render-inline? my-links) ctx)]
     #_[:pre (pr-str (:value ctx))]
     (render-inline-links (filter :link/render-inline? my-links) ctx)]))

(defn ref-many-table [maybe-field links props ctx]
  (let [my-links (link/links-lookup' links [(:fe-pos ctx) (-> ctx :attribute :db/ident)])
        [my-links options-link] (link/process-option-links my-links ctx)
        my-links (->> my-links (filter :link/dependent?))]
    (assert (not options-link) "ref-component-many don't have options; todo handle gracefully")
    [:div.value
     #_[:pre (pr-str maybe-field)]
     [:div.anchors (render-links (remove :link/render-inline? my-links) ctx)]
     (render-inline-links (filter :link/render-inline? my-links) ctx)]))

(defn ref-many-component-table [maybe-field links props ctx]
  (let [my-links (link/links-lookup' links [(:fe-pos ctx) (-> ctx :attribute :db/ident)])]
    [:div.value
     [:div.anchors (render-links (remove :link/render-inline? my-links) ctx)]
     (render-inline-links (filter :link/render-inline? my-links) ctx)]))

(defn multi-select-ref [maybe-field links props ctx]
  (assert false "todo")
  #_(let [add-item! #((:user-with! ctx) (tx/edit-entity (:db/id (:cell-data ctx)) (:attribute ctx) [] [nil]))]
      (multi-select* multi-select-markup add-item! maybe-field links props ctx))) ;add-item! is: add nil to set

;(defn multi-select-ref-component [maybe-field links props ctx]
;  (let [add-item! #((:user-swap! ctx) {:tx (tx/edit-entity (:db/id (:cell-data ctx)) (:attribute ctx) [] [(temp-id!)])})]
;    [multi-select* multi-select-markup add-item! maybe-field links props ctx])) ;add new entity to set

(defn ^:export code [& args]
  (fn [maybe-field links props ctx]
    (let [my-links (link/links-lookup' links [(:fe-pos ctx) (-> ctx :attribute :db/ident)])
          change! #((:user-with! ctx) (tx/update-entity-attr (:cell-data ctx) (:attribute ctx) %))]
      ;^{:key ident}
      [:div.value
       [:div.anchors (render-links (remove :link/render-inline? my-links) ctx)]
       (let [widget (case (:layout ctx) :block code-editor/code-block
                                        :inline-block code-editor/code-inline-block
                                        :table code-editor/code-inline-block)]
         [widget props (:value ctx) change!])               ; backwards args - props last
       (render-inline-links (filter :link/render-inline? my-links) ctx)])))

(defn ^:export markdown [maybe-field links props ctx]
  (let [props (assoc props :mode "markdown" :lineWrapping true)]
    [code maybe-field links props ctx]))

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
        widget (case (:layout ctx) :block edn-block
                                   :inline-block edn-inline-block
                                   :table edn-inline-block)
        my-links (link/links-lookup' links [(:fe-pos ctx) (-> ctx :attribute :db/ident)])
        [my-links options-link] (link/process-option-links my-links ctx)
        my-links (->> my-links (filter :link/dependent?))]
    [:div.value
     [:div.anchors (render-links (remove :link/render-inline? my-links) ctx)]
     [widget value change! props]
     (render-inline-links (filter :link/render-inline? my-links) ctx)]))

(defn edn [maybe-field links props ctx]
  (let [valueType (-> ctx :attribute :db/valueType :db/ident)
        value (if (= valueType :db.type/ref) (:db/id (:value ctx)) (:value ctx))
        change! #((:user-with! ctx) (tx/update-entity-attr (:cell-data ctx) (:attribute ctx) %))
        widget (case (:layout ctx) :block edn-block
                                   :inline-block edn-inline-block
                                   :table edn-inline-block)
        my-links (link/links-lookup' links [(:fe-pos ctx) (-> ctx :attribute :db/ident)])
        [my-links options-link] (link/process-option-links my-links ctx)
        my-links (->> my-links (filter :link/dependent?))]
    [:div.value
     [:div.anchors (render-links (remove :link/render-inline? my-links) ctx)]
     [widget (:value ctx) change! props]
     (render-inline-links (filter :link/render-inline? my-links) ctx)]))

(defn instant [maybe-field links props ctx]
  (let [my-links (link/links-lookup' links [(:fe-pos ctx) (-> ctx :attribute :db/ident)])]
    [:div.value
     [:div.anchors (render-links (remove :link/render-inline? my-links) ctx)]
     (let [change! #((:user-with! ctx) (tx/update-entity-attr (:cell-data ctx) (:attribute ctx) %))
           widget (case (:layout ctx) :block date*
                                      :inline-block edn-inline-block
                                      :table edn-inline-block)]
       [widget (:value ctx) change! props])
     (render-inline-links (filter :link/render-inline? my-links) ctx)]))

(defn text [maybe-field links props ctx]
  (let [my-links (link/links-lookup' links [(:fe-pos ctx) (-> ctx :attribute :db/ident)])]
    [:div.value
     [:div.anchors (render-links (remove :link/render-inline? my-links) ctx)]
     [:span.text
      (case (-> (:attribute ctx) :db/cardinality :db/ident)
        :db.cardinality/many (map pr-str (:value ctx))
        (pr-str (:value ctx)))]
     (render-inline-links (filter :link/render-inline? my-links) ctx)]))
