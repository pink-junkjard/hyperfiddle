(ns hypercrud.ui.widget
  (:refer-clojure :exclude [keyword long boolean])
  (:require [cats.monad.exception :as exception]
            [hypercrud.browser.core :as browser]
            [hypercrud.browser.anchor :as anchor]
            [hypercrud.client.tx :as tx]
            [hypercrud.form.option :as option]
            [hypercrud.ui.code-editor :refer [code-editor*]]
            [hypercrud.ui.input :as input]
            [hypercrud.ui.multi-select :refer [multi-select* multi-select-markup]]
            [hypercrud.ui.radio :as radio]                  ; used in user renderers
            [hypercrud.ui.select :refer [select* select-boolean*]]
            [hypercrud.ui.textarea :refer [textarea*]]
            [hypercrud.types.DbId :refer [->DbId]]
            [re-com.core :as re-com]
            [reagent.core :as r]))


(defn render-anchors
  ([anchor-ctx-pairs]
   (->> anchor-ctx-pairs
        ; Don't filter hidden links; because they could be broken or invalid and need to draw error.
        (map (fn [[anchor param-ctx]]
                (let [prompt (or (:anchor/prompt anchor)
                                 (:anchor/ident anchor)
                                 "_")]
                  ^{:key (hash anchor)}                     ; not a great key but syslinks don't have much.
                  [(:navigate-cmp param-ctx) (anchor/build-anchor-props anchor param-ctx) prompt])))))
  ([anchors param-ctx]
   (render-anchors (map vector anchors (repeat param-ctx)))))

(defn render-inline-anchors
  ([anchors param-ctx]
   (render-inline-anchors (map vector anchors (repeatedly (constantly param-ctx)))))
  ([anchor-ctx-pairs]
   (->> anchor-ctx-pairs
        ; Don't filter hidden links; because they could be broken or invalid and need to draw error.
        (map (fn [[anchor param-ctx]]
               ; don't test anchor validity, we need to render the failure. If this is a dependent link, use visibility predicate to hide the error.
               [:div {:key (hash anchor)}                   ; extra div bc had trouble getting keys to work
                (case (:display-mode param-ctx)
                  :xray (render-anchors [(assoc anchor :anchor/prompt (or (:anchor/prompt anchor) (-> anchor :anchor/link :link/name)))] param-ctx) nil)
                [browser/safe-ui anchor (update param-ctx :debug #(str % ">inline-link[" (:db/id anchor) ":" (:anchor/prompt anchor) "]"))]]))
        (remove nil?)
        (doall))))

(defn option-anchor? [anchor]
  ; don't care if its inline or not, just do the right thing.
  (= :options (:anchor/ident anchor)))

(defn popover-anchor? [anchor]
  (:anchor/managed? anchor))

(defn keyword [maybe-field anchors props param-ctx]
  (let [on-change! #((:user-with! param-ctx) (tx/update-entity-attr (:entity param-ctx) (:attribute param-ctx) %))]
    [input/keyword-input* (:value param-ctx) on-change! props]))


(defn string [maybe-field anchors props param-ctx]
  (let [on-change! #((:user-with! param-ctx) (tx/update-entity-attr (:entity param-ctx) (:attribute param-ctx) %))]
    [input/input* (:value param-ctx) on-change! props]))


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
   [:div.editable-select {:key (:attribute/ident (:attribute param-ctx))}
    [:div.anchors (render-anchors (remove :anchor/render-inline? anchors) param-ctx)]
    (select-boolean* (:value param-ctx) props param-ctx)]
   (render-inline-anchors (filter :anchor/render-inline? anchors) param-ctx)])


(defn dbid [props param-ctx]
  (let [on-change! #((:user-with! param-ctx) (tx/update-entity-attr (:entity param-ctx) (:attribute param-ctx) %))]
    (input/dbid-input (:value param-ctx) on-change! props)))

(defn process-option-anchors [anchors param-ctx]
  (let [[options-anchor] (filter option-anchor? anchors)
        anchors (remove option-anchor? anchors)
        anchors (if (and options-anchor (= :xray (:display-mode param-ctx)))
                  (conj anchors (assoc options-anchor :anchor/render-inline? false))
                  anchors)]
    [anchors options-anchor]))

(defn process-popover-anchors [anchors param-ctx]
  (concat
    (->> anchors (remove popover-anchor?))
    (->> anchors (filter popover-anchor?) (mapv #(assoc % :anchor/render-inline? false)))))

(defn process-option-popover-anchors [anchors param-ctx]
  (process-option-anchors (process-popover-anchors anchors param-ctx) param-ctx))

; this can be used sometimes, on the entity page, but not the query page
(defn ref [maybe-field anchors props param-ctx]
  (let [[anchors options-anchor] (process-option-popover-anchors anchors param-ctx)
        anchors (->> anchors (filter :anchor/repeating?))]
    [:div.value
     ; todo this key is encapsulating other unrelated anchors
     [:div.editable-select {:key (hash (:anchor/link options-anchor))} ; not sure if this is okay in nil field case, might just work
      [:div.anchors (render-anchors (remove :anchor/render-inline? anchors) param-ctx)] ;todo can this be lifted out of editable-select?
      (if options-anchor
        (select* (:value param-ctx) options-anchor props param-ctx)
        (dbid props param-ctx))]
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

(defn ref-many [maybe-field anchors props param-ctx]
  (let [[options-anchor] (filter option-anchor? anchors)
        initial-select (some-> options-anchor               ; not okay to auto-select.
                               (option/hydrate-options' param-ctx)
                               (exception/extract nil)      ; todo handle exception
                               first
                               first)
        select-value-atom (r/atom initial-select)]
    (fn [maybe-field anchors props param-ctx]
      (let [[anchors options-anchor] (process-option-popover-anchors anchors param-ctx)
            anchors (->> anchors (filter :anchor/repeating?))]
        [:div.value
         [:div.anchors (render-anchors (remove :anchor/render-inline? anchors) param-ctx)]
         [:ul
          (->> (:value param-ctx)
               (map (fn [v]
                      [:li {:key (:db/id v)}
                       (:db/id v)                           ; todo remove button
                       ])))]
         [:div.table-controls
          (if options-anchor
            (let [props {:value (-> @select-value-atom :id str)
                         :on-change #(let [select-value (.-target.value %)
                                           dbid (when (not= "" select-value)
                                                  (->DbId (js/parseInt select-value 10) (get-in param-ctx [:entity :db/id :conn-id])))]
                                       (reset! select-value-atom dbid))}
                  ; need lower level select component that can be reused here and in select.cljs
                  select-options (->> (exception/extract (option/hydrate-options' options-anchor param-ctx) nil) ;todo handle exception
                                      (map (fn [[dbid label-prop]]
                                             [:option {:key (:id dbid) :value (-> dbid :id str)} label-prop])))]
              [:select props select-options])
            ; todo wire input to up arrow
            #_(dbid props param-ctx))
          [:br]
          [:button {:on-click #((:user-with! param-ctx) (tx/edit-entity (get-in param-ctx [:entity :db/id]) (-> param-ctx :attribute :attribute/ident) [] [@select-value-atom]))} "⬆"]]
         (render-inline-anchors (filter :anchor/render-inline? anchors) param-ctx)]))))

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

(defn code-block [props change! param-ctx]
  (let [props (if-not (nil? (:read-only props))
                (-> props
                    (dissoc :read-only)
                    (assoc :readOnly (:read-only props)))
                props)]
    [code-editor* (:value param-ctx) change! props]))

(defn code-inline-block [& args]
  (let [showing? (r/atom false)]
    (fn [props change! param-ctx]
      [:div
       [re-com/popover-anchor-wrapper
        :showing? showing?
        :position :below-center
        :anchor [:a {:href "javascript:void 0;" :on-click #(swap! showing? not)} "edit"]
        :popover [re-com/popover-content-wrapper
                  :close-button? true
                  :on-cancel #(reset! showing? false)
                  :no-clip? true
                  :width "600px"
                  :body (code-block props change! param-ctx)]]
       " " (:value param-ctx)])))

(defn code [& args]
  (fn [maybe-field anchors props param-ctx]
    (let [ident (-> param-ctx :attribute :attribute/ident)
          change! #((:user-with! param-ctx) (tx/edit-entity (:db/id (:entity param-ctx)) ident [(:value param-ctx)] [%]))]
      ^{:key ident}
      [:div.value
       (render-inline-anchors (filter :anchor/render-inline? anchors) param-ctx)
       (let [widget (case (:layout param-ctx) :block code-block :inline-block code-inline-block :table code-inline-block)]
         [widget props change! param-ctx])
       [:div.anchors (render-anchors (remove :anchor/render-inline? anchors) param-ctx)]])))

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
    (case (-> (:attribute param-ctx) :attribute/cardinality :db/ident)
      :db.cardinality/many (map pr-str (:value param-ctx))
      (pr-str (:value param-ctx)))]
   (render-inline-anchors (filter :anchor/render-inline? anchors) param-ctx)
   [:div.anchors (render-anchors (remove :anchor/render-inline? anchors) param-ctx)]])

(defn default [maybe-field anchors props param-ctx]
  (let [{:keys [:attribute/valueType :attribute/cardinality :attribute/isComponent]} (:attribute param-ctx)]
    [input/input*
     (str {:valueType (:db/ident valueType)
           :cardinality (:db/ident cardinality)
           :isComponent isComponent})
     #()
     {:read-only true}]))

(defn raw [_ anchors props param-ctx]
  (let [valueType (-> param-ctx :attribute :attribute/valueType :db/ident)
        value (if (= valueType :db.type/ref) (:db/id (:value param-ctx)) (:value param-ctx))
        on-change! #((:user-with! param-ctx) (tx/update-entity-attr (:entity param-ctx) (:attribute param-ctx) %))]
    [input/edn-input* value on-change! props]))
