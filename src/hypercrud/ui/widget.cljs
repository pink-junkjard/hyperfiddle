(ns hypercrud.ui.widget
  (:refer-clojure :exclude [keyword long boolean])
  (:require [cats.monad.either :as either]
            [clojure.set :as set]
            [hypercrud.browser.anchor :as anchor]
            [hypercrud.browser.core :as browser]
            [hypercrud.client.tx :as tx]
            [hypercrud.ui.code-editor :as code-editor]
            [hypercrud.ui.input :as input]
            [hypercrud.ui.multi-select :refer [multi-select* multi-select-markup]]
            [hypercrud.ui.radio]                            ; used in user renderers
            [hypercrud.ui.select :refer [select* select-boolean*]]
            [hypercrud.ui.textarea :refer [textarea*]]
            [hypercrud.util.core :refer [pprint-str]]
            [hypercrud.util.string :refer [safe-read-string]]))


(defn render-anchor [anchor ctx]
  (let [prompt (or (:anchor/prompt anchor)
                   (:anchor/ident anchor)
                   "_")]
    [(:navigate-cmp ctx) (anchor/build-anchor-props anchor ctx) prompt]))

(defn render-anchors
  ([anchor-ctx-pairs]
   (->> anchor-ctx-pairs
        ; Don't filter hidden links; because they could be broken or invalid and need to draw error.
        (map (fn [[anchor ctx]]
               ^{:key (hash (:db/id anchor))}
               [render-anchor anchor ctx]))
        doall))
  ([anchors param-ctx]
   (render-anchors (map vector anchors (repeat param-ctx)))))

(defn render-inline-anchors
  ([anchors param-ctx]
   (render-inline-anchors (map vector anchors (repeat param-ctx))))
  ([anchor-ctx-pairs]
   (->> anchor-ctx-pairs
        ; Don't filter hidden links; because they could be broken or invalid and need to draw error.
        (map (fn [[anchor param-ctx]]
               ; don't test anchor validity, we need to render the failure. If this is a dependent link, use visibility predicate to hide the error.
               [:div {:key (hash (:db/id anchor))}          ; extra div bc had trouble getting keys to work
                ; NOTE: this param-ctx logic and structure is the same as the inline branch of browser-request/recurse-request
                [browser/ui anchor (update param-ctx :debug #(str % ">inline-link[" (:db/id anchor) ":" (or (:anchor/ident anchor) (:anchor/prompt anchor)) "]"))]]))
        (remove nil?)
        (doall))))

(defn keyword [maybe-field anchors props param-ctx]
  (let [on-change! #((:user-with! param-ctx) (tx/update-entity-attr (:entity param-ctx) (:attribute param-ctx) %))]
    [input/keyword-input* (:value param-ctx) on-change! props]))

(defn string [maybe-field anchors props param-ctx]
  [:div.value
   [:div.anchors (render-anchors (remove :anchor/render-inline? anchors) param-ctx)]
   (let [on-change! #((:user-with! param-ctx) (tx/update-entity-attr (:entity param-ctx) (:attribute param-ctx) %))]
     [input/input* (:value param-ctx) on-change! props])
   (render-inline-anchors (filter :anchor/render-inline? anchors) param-ctx)])

(defn long [maybe-field anchors props param-ctx]
  [:div.value
   [:div.anchors (render-anchors (remove :anchor/render-inline? anchors) param-ctx)]
   [input/validated-input
    (:value param-ctx) #((:user-with! param-ctx) (tx/update-entity-attr (:entity param-ctx) (:attribute param-ctx) %))
    #(js/parseInt % 10) pr-str
    #(or (integer? (js/parseInt % 10)) (= "nil" %))
    props]
   (render-inline-anchors (filter :anchor/render-inline? anchors) param-ctx)])

(defn textarea [maybe-field anchors props param-ctx]
  (let [set-attr! #((:user-with! param-ctx) (tx/update-entity-attr (:entity param-ctx) (:attribute param-ctx) %))]
    [textarea* (merge {:type "text"
                       :value (:value param-ctx)
                       :on-change set-attr!}
                      props)]))

(defn boolean [maybe-field anchors props param-ctx]
  [:div.value
   [:div.editable-select {:key (:db/ident (:attribute param-ctx))}
    [:div.anchors (render-anchors (remove :anchor/render-inline? anchors) param-ctx)]
    (select-boolean* (:value param-ctx) props param-ctx)]
   (render-inline-anchors (filter :anchor/render-inline? anchors) param-ctx)])

(defn dbid [props param-ctx]
  (let [on-change! #((:user-with! param-ctx) (tx/update-entity-attr (:entity param-ctx) (:attribute param-ctx) %))]
    (input/dbid-input (:value param-ctx) on-change! props)))

(defn process-option-anchors [anchors param-ctx]
  (let [[options-anchor] (filter anchor/option-anchor? anchors)
        anchors (remove anchor/option-anchor? anchors)]
    [anchors options-anchor]))

(defn process-popover-anchor [anchor]
  (if (anchor/popover-anchor? anchor)
    (assoc anchor :anchor/render-inline? false)
    anchor))

(defn process-popover-anchors [anchors param-ctx]
  (mapv process-popover-anchor anchors))

(defn process-option-popover-anchors [anchors param-ctx]
  (process-option-anchors (process-popover-anchors anchors param-ctx) param-ctx))

; this can be used sometimes, on the entity page, but not the query page
(defn ref [maybe-field anchors props param-ctx]
  (let [[anchors options-anchor] (process-option-popover-anchors anchors param-ctx)
        anchors (->> anchors (filter :anchor/repeating?))]
    [:div.value
     [:div.editable-select
      [:div.anchors (render-anchors (remove :anchor/render-inline? anchors) param-ctx)] ;todo can this be lifted out of editable-select?
      [:div.select                                          ; helps the weird anchor float left css thing
       (if options-anchor
         (select* (:value param-ctx) options-anchor props param-ctx)
         (dbid props param-ctx))]]
     (render-inline-anchors (filter :anchor/render-inline? anchors) param-ctx)]))

(defn ref-component [maybe-field anchors props param-ctx]
  (let [[anchors options-anchor] (process-option-popover-anchors anchors param-ctx)
        anchors (->> anchors (filter :anchor/repeating?))]
    (assert (not options-anchor) "ref-components don't have options; todo handle gracefully")
    #_(assert (> (count (filter :anchor/render-inline? anchors)) 0))
    #_(ref maybe-field anchors props param-ctx)
    [:div.value
     #_[:pre (pr-str (:value param-ctx))]
     [:div.anchors (render-anchors (remove :anchor/render-inline? anchors) param-ctx)]
     (render-inline-anchors (filter :anchor/render-inline? anchors) param-ctx)]))

(defn ref-many-table [maybe-field anchors props param-ctx]
  (let [[anchors options-anchor] (process-option-popover-anchors anchors param-ctx)
        anchors (->> anchors (filter :anchor/repeating?))]
    (assert (not options-anchor) "ref-component-many don't have options; todo handle gracefully")
    [:div.value
     #_[:pre (pr-str maybe-field)]
     [:div.anchors (render-anchors (remove :anchor/render-inline? anchors) param-ctx)]
     (render-inline-anchors (filter :anchor/render-inline? anchors) param-ctx)]))

(defn ref-many-component-table [maybe-field anchors props param-ctx]
  [:div.value
   (render-inline-anchors (filter :anchor/render-inline? anchors) param-ctx)
   [:div.anchors (render-anchors (remove :anchor/render-inline? anchors) param-ctx)]])

(defn multi-select-ref [maybe-field anchors props param-ctx]
  (let [add-item! #((:user-with! param-ctx) (tx/edit-entity (:db/id (:entity param-ctx)) (:attribute param-ctx) [] [nil]))]
    (multi-select* multi-select-markup add-item! maybe-field anchors props param-ctx))) ;add-item! is: add nil to set

;(defn multi-select-ref-component [maybe-field anchors props param-ctx]
;  (let [add-item! #((:user-swap! param-ctx) {:tx (tx/edit-entity (:db/id (:entity param-ctx)) (:attribute param-ctx) [] [(temp-id!)])})]
;    [multi-select* multi-select-markup add-item! maybe-field anchors props param-ctx])) ;add new entity to set

(defn code [& args]
  (fn [maybe-field anchors props param-ctx]
    (let [ident (-> param-ctx :attribute :db/ident)
          change! #((:user-with! param-ctx) (tx/update-entity-attr (:entity param-ctx) (:attribute param-ctx) %))]
      ;^{:key ident}
      [:div.value
       (render-inline-anchors (filter :anchor/render-inline? anchors) param-ctx)
       (let [widget (case (:layout param-ctx) :block code-editor/code-block
                                              :inline-block code-editor/code-inline-block
                                              :table code-editor/code-inline-block)]
         [widget props (:value param-ctx) change!])
       [:div.anchors (render-anchors (remove :anchor/render-inline? anchors) param-ctx)]])))

(defn markdown [maybe-field anchors props param-ctx]
  (let [props (assoc props
                :mode "markdown"
                :lineWrapping true)]
    [code maybe-field anchors props param-ctx]))

(defn edn-many [maybe-field anchors props ctx]
  (let [valueType (-> ctx :attribute :db/valueType :db/ident)
        value (-> (if (= valueType :db.type/ref)
                    (map :db/id (:value ctx))
                    (:value ctx))
                  set)
        change! (fn [user-edn-str]
                  (either/branch
                    (safe-read-string user-edn-str)
                    (fn [e] (js/console.error (pr-str e)) nil)
                    (fn [user-val]
                      (let [user-val (set user-val)
                            rets (set/difference value user-val)
                            adds (set/difference user-val value)]
                        ((:user-with! ctx) (tx/edit-entity (-> ctx :entity :db/id)
                                                           (-> ctx :attribute :db/ident)
                                                           rets adds))))))
        [anchors options-anchor] (process-option-popover-anchors anchors ctx)
        anchors (->> anchors (filter :anchor/repeating?))]
    [:div.value
     (render-inline-anchors (filter :anchor/render-inline? anchors) ctx)
     [code-editor/code-inline-block props (pprint-str value) change!]
     [:div.anchors (render-anchors (remove :anchor/render-inline? anchors) ctx)]]))

(defn edn [maybe-field anchors props param-ctx]
  (let [valueType (-> param-ctx :attribute :db/valueType :db/ident)
        value (if (= valueType :db.type/ref) (:db/id (:value param-ctx)) (:value param-ctx))
        change! (fn [user-edn-str]
                  (either/branch
                    (safe-read-string user-edn-str)
                    (fn [e] (js/console.error (pr-str e)) nil)
                    #((:user-with! param-ctx) (tx/update-entity-attr (:entity param-ctx) (:attribute param-ctx) %))))
        [anchors options-anchor] (process-option-popover-anchors anchors param-ctx)
        anchors (->> anchors (filter :anchor/repeating?))]
    [:div.value
     (render-inline-anchors (filter :anchor/render-inline? anchors) param-ctx)
     [code-editor/code-inline-block props (pprint-str value) change!]
     [:div.anchors (render-anchors (remove :anchor/render-inline? anchors) param-ctx)]]))

(defn valid-date-str? [s]
  (or (empty? s)
      (let [ms (.parse js/Date s)]                          ; NaN if not valid string
        (integer? ms))))

(defn instant [maybe-field anchors props param-ctx]
  (let [on-change! #((:user-with! param-ctx) (tx/update-entity-attr (:entity param-ctx) (:attribute param-ctx) %))
        parse-string (fn [s]
                       (if (empty? s)
                         nil
                         (let [ms (.parse js/Date s)]
                           (js/Date. ms))))
        to-string #(some-> % .toISOString)]
    [input/validated-input (:value param-ctx) on-change! parse-string to-string valid-date-str? props]))

(defn text [maybe-field anchors props param-ctx]
  [:div.value
   [:span.text
    (case (-> (:attribute param-ctx) :db/cardinality :db/ident)
      :db.cardinality/many (map pr-str (:value param-ctx))
      (pr-str (:value param-ctx)))]
   (render-inline-anchors (filter :anchor/render-inline? anchors) param-ctx)
   [:div.anchors (render-anchors (remove :anchor/render-inline? anchors) param-ctx)]])

(defn default [maybe-field anchors props param-ctx]
  (let [{:keys [:db/valueType :db/cardinality :db/isComponent]} (:attribute param-ctx)]
    [input/input*
     (str {:valueType (:db/ident valueType)
           :cardinality (:db/ident cardinality)
           :isComponent isComponent})
     #()
     {:read-only true}]))
