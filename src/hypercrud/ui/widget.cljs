(ns hypercrud.ui.widget
  (:refer-clojure :exclude [keyword long boolean])
  (:require [cljs.reader :as reader]
            [hypercrud.browser.core :as browser]
            [hypercrud.browser.links :as links]
            [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx]
            [hypercrud.form.option :as option]
            [hypercrud.form.q-util :as q-util]
            [hypercrud.ui.code-editor :refer [code-editor*]]
            [hypercrud.ui.input :as input]
            [hypercrud.ui.multi-select :refer [multi-select* multi-select-markup]]
            [hypercrud.ui.radio :as radio]
            [hypercrud.ui.select :refer [select* select-boolean*]]
            [hypercrud.ui.textarea :refer [textarea*]]))


(defn render-anchors
  ([anchor-ctx-pairs]
   (->> anchor-ctx-pairs
        (filter (partial apply links/link-visible?))
        (mapv (fn [[anchor param-ctx]]
                (assert (:navigate-cmp param-ctx))
                ^{:key (hash anchor)}
                [(:navigate-cmp param-ctx) (links/build-link-props anchor param-ctx) (:anchor/prompt anchor) param-ctx]))
        (interpose " · ")))
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
                 ^{:key (hash anchor)}
                 [browser/ui params-map ui-param-ctx]))))))


(defn keyword [value maybe-field anchors props param-ctx]
  (let [on-change! #((:user-swap! param-ctx) {:tx (tx/update-entity-attr (:entity param-ctx) (:attribute param-ctx) %)})]
    [input/keyword-input* value on-change! props]))


(defn string [value maybe-field anchors props param-ctx]
  (let [on-change! #((:user-swap! param-ctx) {:tx (tx/update-entity-attr (:entity param-ctx) (:attribute param-ctx) %)})]
    [input/input* value on-change! props]))


(defn long [value maybe-field anchors props param-ctx]
  [input/validated-input
   value #((:user-swap! param-ctx) {:tx (tx/update-entity-attr (:entity param-ctx) (:attribute param-ctx) %)})
   #(js/parseInt % 10) pr-str
   #(integer? (js/parseInt % 10))
   props])


(defn textarea [value maybe-field anchors props param-ctx]
  (let [set-attr! #((:user-swap! param-ctx) {:tx (tx/update-entity-attr (:entity param-ctx) (:attribute param-ctx) %)})]
    [textarea* (merge {:type "text"
                       :value value
                       :on-change set-attr!}
                      props)]))


(defn radio-ref [value maybe-field anchors props param-ctx]
  ;;radio* needs parameterized markup fn todo
  [radio/radio-ref* value maybe-field props param-ctx])


(defn boolean [value maybe-field anchors props param-ctx]
  [:div.value
   [:div.editable-select {:key (:attribute/ident (:attribute param-ctx))}
    [:div.anchors (render-anchors (remove :anchor/render-inline? anchors) param-ctx)]
    (select-boolean* value props param-ctx)]
   (render-inline-links maybe-field (filter :anchor/render-inline? anchors) param-ctx)])


(defn dbid [value props param-ctx]
  (let [on-change! #((:user-swap! param-ctx) {:tx (tx/update-entity-attr (:entity param-ctx) (:attribute param-ctx) %)})]
    (input/dbid-input value on-change! props)))


; this can be used sometimes, on the entity page, but not the query page
(defn ref [value maybe-field anchors props param-ctx]
  [:div.value
   [:div.editable-select {:key (option/get-hydrate-key maybe-field)} ; not sure if this is okay in nil field case, might just work
    #_[:div.anchors (render-anchors (remove :anchor/render-inline? anchors) param-ctx)]
    (if maybe-field
      (select* value maybe-field props param-ctx)
      (dbid value props param-ctx))]
   (render-inline-links maybe-field (filter :anchor/render-inline? anchors) param-ctx)])


(defn ref-component [value maybe-field anchors props param-ctx]
  (assert (> (count (filter :anchor/render-inline? anchors)) 0))
  #_(ref value maybe-field anchors props param-ctx)
  [:div.value
   #_(pr-str (:db/id value))
   (render-inline-links maybe-field (filter :anchor/render-inline? anchors) param-ctx)
   [:div.anchors (render-anchors (remove :anchor/render-inline? anchors) param-ctx)]])


(defn ref-many-table [value maybe-field anchors props param-ctx]
  [:div.value
   (render-inline-links maybe-field (filter :anchor/render-inline? anchors) param-ctx)
   [:div.anchors (render-anchors (remove :anchor/render-inline? anchors) param-ctx)]])


(comment
  (let [initial-select (let [result (first (option/hydrate-options field param-ctx))]
                         (assert (= 1 (count result)) "Cannot use multiple find-elements for an options-link")
                         (first result))
        select-value-atom (r/atom (:db/id initial-select))]
    (fn [entity {:keys [field graph navigate-cmp user-swap!]}]
      (let [ident (-> field :field/attribute :attribute/ident)
            resultset (mapv vector (get entity ident))]
        [:div.value
         [table/table graph resultset (vector (:field/form field)) user-swap! navigate-cmp] ; busto
         (let [props {:value (str @select-value-atom)
                      :on-change #(let [select-value (.-target.value %)
                                        value (reader/read-string select-value)]
                                    (reset! select-value-atom value))}
               ; need lower level select component that can be reused here and in select.cljs
               select-options (->> (option/hydrate-options field param-ctx)
                                   (sort-by second)
                                   (map (fn [[dbid label-prop]]
                                          [:option {:key (hash dbid) :value (pr-str dbid)} label-prop])))]
           [:div.table-controls
            [:select props select-options]
            [:button {:on-click #(user-swap! {:tx (tx/edit-entity (:db/id entity) ident [] [@select-value-atom])})} "⬆"]])]))))

(defn ref-many-component-table [value maybe-field anchors props param-ctx]
  [:div.value
   (render-inline-links maybe-field (filter :anchor/render-inline? anchors) param-ctx)
   [:div.anchors (render-anchors (remove :anchor/render-inline? anchors) param-ctx)]])

(defn multi-select-ref [value maybe-field anchors props param-ctx]
  (let [add-item! #((:user-swap! param-ctx) {:tx (tx/edit-entity (:db/id (:entity param-ctx)) (:attribute param-ctx) [] [nil])})]
    (multi-select* multi-select-markup value add-item! maybe-field anchors props param-ctx))) ;add-item! is: add nil to set

(defn multi-select-ref-component [value maybe-field anchors props param-ctx]
  (let [temp-id! (partial hc/*temp-id!* (-> (:entity param-ctx) :db/id :conn-id)) ; bound to fix render bug
        add-item! #((:user-swap! param-ctx) {:tx (tx/edit-entity (:db/id (:entity param-ctx)) (:attribute param-ctx) [] [(temp-id!)])})]
    [multi-select* multi-select-markup value add-item! maybe-field anchors props param-ctx])) ;add new entity to set

(defn code [value maybe-field anchors props param-ctx]
  (let [ident (-> param-ctx :attribute :attribute/ident)
        change! #((:user-swap! param-ctx) {:tx (tx/edit-entity (:db/id (:entity param-ctx)) ident [value] [%])})]
    ^{:key ident}
    [:div.value
     (render-inline-links maybe-field (filter :anchor/render-inline? anchors) param-ctx)
     (case (:layout param-ctx)
       :form (let [props (if-not (nil? (:read-only props))
                           (-> props
                               (dissoc :read-only)
                               (assoc :readOnly (if (:read-only props)
                                                  "nocursor"
                                                  false)))
                           props)]
               [code-editor* value change! props])
       [:noscript])
     [:div.anchors (render-anchors (remove :anchor/render-inline? anchors) param-ctx)]]))

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
