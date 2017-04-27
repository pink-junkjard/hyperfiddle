(ns hypercrud.ui.widget
  (:refer-clojure :exclude [keyword long boolean])
  (:require [cats.monad.exception :as exception]
            [hypercrud.browser.core :as browser]
            [hypercrud.browser.links :as links]
            [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx]
            [hypercrud.form.option :as option]
            [hypercrud.ui.code-editor :refer [code-editor*]]
            [hypercrud.ui.input :as input]
            [hypercrud.ui.multi-select :refer [multi-select* multi-select-markup]]
            [hypercrud.ui.radio :as radio]                  ; used in user renderers
            [hypercrud.ui.select :refer [select* select-boolean*]]
            [hypercrud.ui.textarea :refer [textarea*]]
            [hypercrud.types :refer [->DbId]]
            [re-com.core :as re-com :refer-macros [handler-fn]]
            [reagent.core :as r]))


(defn render-anchors
  ([anchor-ctx-pairs]
   (->> anchor-ctx-pairs
        (filter (partial apply links/link-visible?))
        (mapv (fn [[anchor param-ctx]]
                (assert (:navigate-cmp param-ctx))
                (let [prompt (or (:anchor/prompt anchor)
                                 (:anchor/ident anchor)
                                 "_")]
                  ^{:key (hash anchor)}                     ; not a great key but syslinks don't have much.
                  [(:navigate-cmp param-ctx) (links/build-link-props anchor param-ctx) prompt])))
        (interpose " ")))
  ([anchors param-ctx]
   (render-anchors (map vector anchors (repeat param-ctx)))))


(defn render-inline-links
  ([maybe-field anchors param-ctx]                          ; the unused param on this airity is concerning
   (render-inline-links anchors (assoc param-ctx :isComponent (:attribute/isComponent (:attribute param-ctx)))))
  ([anchors param-ctx]
   (render-inline-links (map vector anchors (repeatedly (constantly param-ctx)))))
  ([anchor-ctx-pairs]
   (->> anchor-ctx-pairs
        (filter (partial apply links/link-visible?))
        (map (fn [[anchor param-ctx]]
               (let [params-map (links/build-url-params-map anchor param-ctx)
                     ui-param-ctx (-> param-ctx
                                      (update :debug #(str % ">inline-link[" (:db/id anchor) ":" (:anchor/prompt anchor) "]"))
                                      (dissoc :result "entity"))]

                 [:div {:key (hash anchor)}               ; extra div bc had trouble getting keys to work
                  (case (:display-mode param-ctx) :xray (render-anchors [(assoc anchor :anchor/prompt "self")] param-ctx) nil)
                  [browser/safe-ui params-map ui-param-ctx]])))
        (doall))))

(defn option-anchor? [anchor]
  ; don't care if its inline or not, just do the right thing.
  (= :options (:anchor/ident anchor)))

(defn popover-anchor? [anchor]
  (let [{r :anchor/repeating? a :anchor/attribute} anchor]
    (and (not r) a)))

(defn keyword [value maybe-field anchors props param-ctx]
  (let [on-change! #((:user-swap! param-ctx) {:tx (tx/update-entity-attr (:entity param-ctx) (:attribute param-ctx) %)})]
    [input/keyword-input* value on-change! props]))


(defn string [value maybe-field anchors props param-ctx]
  (let [on-change! #((:user-swap! param-ctx) {:tx (tx/update-entity-attr (:entity param-ctx) (:attribute param-ctx) %)})]
    [input/input* value on-change! props]))


(defn long [value maybe-field anchors props param-ctx]
  [:div.value
   [:div.anchors (render-anchors (remove :anchor/render-inline? anchors) param-ctx)]
   [input/validated-input
    value #((:user-swap! param-ctx) {:tx (tx/update-entity-attr (:entity param-ctx) (:attribute param-ctx) %)})
    #(js/parseInt % 10) pr-str
    #(or (integer? (js/parseInt % 10)) (= "nil" %))
    props]
   (render-inline-links maybe-field (filter :anchor/render-inline? anchors) param-ctx)])


(defn textarea [value maybe-field anchors props param-ctx]
  (let [set-attr! #((:user-swap! param-ctx) {:tx (tx/update-entity-attr (:entity param-ctx) (:attribute param-ctx) %)})]
    [textarea* (merge {:type "text"
                       :value value
                       :on-change set-attr!}
                      props)]))

(defn boolean [value maybe-field anchors props param-ctx]
  [:div.value
   [:div.editable-select {:key (:attribute/ident (:attribute param-ctx))}
    [:div.anchors (render-anchors (remove :anchor/render-inline? anchors) param-ctx)]
    (select-boolean* value props param-ctx)]
   (render-inline-links maybe-field (filter :anchor/render-inline? anchors) param-ctx)])


(defn dbid [value props param-ctx]
  (let [on-change! #((:user-swap! param-ctx) {:tx (tx/update-entity-attr (:entity param-ctx) (:attribute param-ctx) %)})]
    (input/dbid-input value on-change! props)))


(defn process-option-popover-anchors [anchors param-ctx]
  (let [[options-anchor] (filter option-anchor? anchors)
        popover-anchors (filter popover-anchor? anchors)
        anchors (->> anchors
                     (remove option-anchor?)                ; only in xray mode
                     (remove popover-anchor?))              ; ensure inline-false

        ; put the special anchors back as links in xray mode
        anchors (if (and options-anchor (= :xray (:display-mode param-ctx)))
                  (conj anchors (assoc options-anchor :anchor/render-inline? false))
                  anchors)

        anchors (concat anchors (mapv #(assoc % :anchor/render-inline? false) popover-anchors))]
    [anchors options-anchor]))

; this can be used sometimes, on the entity page, but not the query page
(defn ref [value maybe-field anchors props param-ctx]
  (let [[anchors options-anchor] (process-option-popover-anchors anchors param-ctx)]
    [:div.value
     ; todo this key is encapsulating other unrelated anchors
     [:div.editable-select {:key (hash (get-in options-anchor [:anchor/link :link/request]))} ; not sure if this is okay in nil field case, might just work
      [:div.anchors (render-anchors (remove :anchor/render-inline? anchors) param-ctx)] ;todo can this be lifted out of editable-select?
      (if options-anchor
        (select* value options-anchor props param-ctx)
        (dbid value props param-ctx))]
     (render-inline-links maybe-field (filter :anchor/render-inline? anchors) param-ctx)]))


(defn ref-component [value maybe-field anchors props param-ctx]
  (let [[anchors options-anchor] (process-option-popover-anchors anchors param-ctx)]
    (assert (not options-anchor) "ref-components don't have options; todo handle gracefully")
    #_(assert (> (count (filter :anchor/render-inline? anchors)) 0))
    #_(ref value maybe-field anchors props param-ctx)
    [:div.value
     #_[:pre (pr-str value)]
     [:div.anchors (render-anchors (remove :anchor/render-inline? anchors) param-ctx)]
     (render-inline-links maybe-field (filter :anchor/render-inline? anchors) param-ctx)]))


(defn ref-many-table [value maybe-field anchors props param-ctx]
  (let [[anchors options-anchor] (process-option-popover-anchors anchors param-ctx)]
    (assert (not options-anchor) "ref-component-many don't have options; todo handle gracefully")
    [:div.value
     #_[:pre (pr-str maybe-field)]
     [:div.anchors (render-anchors (remove :anchor/render-inline? anchors) param-ctx)]
     (render-inline-links maybe-field (filter :anchor/render-inline? anchors) param-ctx)]))

(defn ref-many [value maybe-field anchors props param-ctx]
  (let [[options-anchor] (filter option-anchor? anchors)
        initial-select (some-> options-anchor               ; not okay to auto-select.
                               (option/hydrate-options param-ctx)
                               (exception/extract nil)      ; todo handle exception
                               first
                               first)
        select-value-atom (r/atom initial-select)]
    (fn [value maybe-field anchors props param-ctx]
      (let [[options-anchor] (filter option-anchor? anchors)
            anchors (remove option-anchor? anchors)
            anchors (if (and options-anchor (= :xray (:display-mode param-ctx)))
                      (conj anchors (assoc options-anchor :anchor/render-inline? false))
                      anchors)]
        [:div.value
         [:div.anchors (render-anchors (remove :anchor/render-inline? anchors) param-ctx)]
         [:ul
          (->> value
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
                                       (.log js/console (pr-str select-value))
                                       (reset! select-value-atom dbid))}
                  ; need lower level select component that can be reused here and in select.cljs
                  select-options (->> (exception/extract (option/hydrate-options options-anchor param-ctx) nil) ;todo handle exception
                                      (map (fn [[dbid label-prop]]
                                             [:option {:key (:id dbid) :value (-> dbid :id str)} label-prop])))]
              [:select props select-options])
            ; todo wire input to up arrow
            #_(dbid value props param-ctx))
          [:br]
          [:button {:on-click #((:user-swap! param-ctx) {:tx (tx/edit-entity (get-in param-ctx [:entity :db/id]) (-> param-ctx :attribute :attribute/ident) [] [@select-value-atom])})} "⬆"]]
         (render-inline-links maybe-field (filter :anchor/render-inline? anchors) param-ctx)]))))

(defn ref-many-component-table [value maybe-field anchors props param-ctx]
  [:div.value
   (render-inline-links maybe-field (filter :anchor/render-inline? anchors) param-ctx)
   [:div.anchors (render-anchors (remove :anchor/render-inline? anchors) param-ctx)]])

(defn multi-select-ref [value maybe-field anchors props param-ctx]
  (let [add-item! #((:user-swap! param-ctx) {:tx (tx/edit-entity (:db/id (:entity param-ctx)) (:attribute param-ctx) [] [nil])})]
    (multi-select* multi-select-markup value add-item! maybe-field anchors props param-ctx))) ;add-item! is: add nil to set

;(defn multi-select-ref-component [value maybe-field anchors props param-ctx]
;  (let [add-item! #((:user-swap! param-ctx) {:tx (tx/edit-entity (:db/id (:entity param-ctx)) (:attribute param-ctx) [] [(temp-id!)])})]
;    [multi-select* multi-select-markup value add-item! maybe-field anchors props param-ctx])) ;add new entity to set

(defn code [& args]
  (let [showing? (r/atom false)]
    (fn [value maybe-field anchors props param-ctx]
      (let [ident (-> param-ctx :attribute :attribute/ident)
            change! #((:user-swap! param-ctx) {:tx (tx/edit-entity (:db/id (:entity param-ctx)) ident [value] [%])})
            code-widget (let [props (if-not (nil? (:read-only props))
                                      (-> props
                                          (dissoc :read-only)
                                          (assoc :readOnly (:read-only props)))
                                      props)]
                          [code-editor* value change! props])]
        ^{:key ident}
        [:div.value
         (render-inline-links maybe-field (filter :anchor/render-inline? anchors) param-ctx)
         (case (:layout param-ctx)
           :form code-widget
           :table [:div
                   [re-com/popover-anchor-wrapper
                    :showing? showing?
                    :position :below-center
                    :anchor [:a {:href "javascript:void 0;"
                                 :on-click #(swap! showing? not)} "edit"]
                    :popover [re-com/popover-content-wrapper
                              :close-button? true
                              :on-cancel #(reset! showing? false)
                              :no-clip? true
                              :width "600px"
                              :body code-widget]]
                   " " value])
         [:div.anchors (render-anchors (remove :anchor/render-inline? anchors) param-ctx)]]))))

(defn valid-date-str? [s]
  (or (empty? s)
      (let [ms (.parse js/Date s)]                          ; NaN if not valid string
        (integer? ms))))

(defn instant [value maybe-field anchors props param-ctx]
  (let [on-change! #((:user-swap! param-ctx) {:tx (tx/update-entity-attr (:entity param-ctx) (:attribute param-ctx) %)})
        parse-string (fn [s]
                       (if (empty? s)
                         nil
                         (let [ms (.parse js/Date s)]
                           (js/Date. ms))))
        to-string #(some-> % .toISOString)]
    [input/validated-input value on-change! parse-string to-string valid-date-str? props]))

(defn text [value maybe-field anchors props param-ctx]
  [:div.value
   [:span.text
    (case (-> (:attribute param-ctx) :attribute/cardinality :db/ident)
      :db.cardinality/many (map pr-str value)
      (pr-str value))]
   (render-inline-links maybe-field (filter :anchor/render-inline? anchors) param-ctx)
   [:div.anchors (render-anchors (remove :anchor/render-inline? anchors) param-ctx)]])

(defn default [value maybe-field anchors props param-ctx]
  (let [{:keys [:attribute/valueType :attribute/cardinality :attribute/isComponent]} (:attribute param-ctx)]
    [input/input*
     (str {:valueType (:db/ident valueType)
           :cardinality (:db/ident cardinality)
           :isComponent isComponent})
     #()
     {:read-only true}]))

(defn raw [value _ anchors props param-ctx]
  (let [valueType (-> param-ctx :attribute :attribute/valueType :db/ident)
        value (if (= valueType :db.type/ref) (:db/id value) value)
        on-change! #((:user-swap! param-ctx) {:tx (tx/update-entity-attr (:entity param-ctx) (:attribute param-ctx) %)})]
    [input/edn-input* value on-change! props]))
