(ns hypercrud.ui.widget
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
            [hypercrud.ui.textarea :refer [textarea*]]
            [reagent.core :as r]))


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
  ([field anchors param-ctx]
   (render-inline-links anchors (assoc param-ctx :isComponent (-> field :field/attribute :attribute/isComponent))))
  ([anchors param-ctx]
   (render-inline-links (map vector anchors (repeatedly (constantly param-ctx)))))
  ([anchor-ctx-pairs]
   (->> anchor-ctx-pairs
        (filter (partial apply links/link-visible?))
        (map (fn [[anchor param-ctx]]
               (let [params-map (links/build-url-params-map anchor param-ctx)
                     ui-param-ctx (-> param-ctx
                                      (update :debug #(str % ">inline-link[" (:db/id anchor) ":" (:anchor/prompt anchor) "]"))
                                      (dissoc :result :entity))]
                 ^{:key (hash anchor)}
                 [browser/ui params-map ui-param-ctx]))))))


(defn input-keyword [value field anchors props {:keys [user-swap!] :as param-ctx}]
  (let [attribute (:field/attribute field)
        on-change! #(user-swap! {:tx (tx/update-entity-attr (:entity param-ctx) attribute %)})]
    [input/keyword-input* value on-change! props]))


(defn input [value field anchors props {:keys [user-swap!] :as param-ctx}]
  (let [attribute (:field/attribute field)
        on-change! #(user-swap! {:tx (tx/update-entity-attr (:entity param-ctx) attribute %)})]
    [input/input* value on-change! props]))


(defn input-long [value field anchors props {:keys [user-swap!] :as param-ctx}]
  (let [attribute (:field/attribute field)]
    [input/validated-input
     value #(user-swap! {:tx (tx/update-entity-attr (:entity param-ctx) attribute %)})
     #(js/parseInt % 10) pr-str
     #(integer? (js/parseInt % 10))
     props]))


(defn textarea [value field anchors props {:keys [user-swap!] :as param-ctx}]
  (let [attribute (:field/attribute field)
        set-attr! #(user-swap! {:tx (tx/update-entity-attr (:entity param-ctx) attribute %)})]
    [textarea* (merge {:type "text"
                       :value value
                       :on-change set-attr!}
                      props)]))


(defn radio-ref [value field anchors props param-ctx]
  ;;radio* needs parameterized markup fn todo
  [radio/radio-ref* value field props param-ctx])


(defn select-boolean [value field anchors props param-ctx]
  (let [{:keys [:attribute/ident]} (:field/attribute field)]
    [:div.value
     [:div.editable-select {:key ident}
      [:div.anchors (render-anchors (remove :anchor/render-inline? anchors) param-ctx)]
      (if (:read-only props)
        [:span.text (case value
                      true "True"
                      false "False"
                      "--")]
        (select-boolean* value field param-ctx))]
     (render-inline-links field (filter :anchor/render-inline? anchors) param-ctx)]))


; this can be used sometimes, on the entity page, but not the query page
(defn select-ref [value field anchors props param-ctx]
  [:div.value
   [:div.editable-select {:key (option/get-key field)}
    [:div.anchors (render-anchors (remove :anchor/render-inline? anchors) param-ctx)]
    (if (:read-only props)
      [:span.text
       (-> value
           (get (-> field :field/attribute :attribute/ident))
           (get (-> field
                    ; we are assuming we have a query link here
                    :field/options-anchor :anchor/link :link/request :link-query/find-element first
                    :find-element/form :form/field first
                    :field/attribute :attribute/ident)))]
      [:span.select
       (select* value field param-ctx)])]
   (render-inline-links field (filter :anchor/render-inline? anchors) param-ctx)])


(defn select-ref-component [value field anchors props param-ctx]
  [:div.value
   #_(pr-str (:db/id value))
   (render-inline-links field (filter :anchor/render-inline? anchors) param-ctx)
   [:div.anchors (render-anchors (remove :anchor/render-inline? anchors) param-ctx)]])


(defn table-many-ref [value field anchors props param-ctx]
  [:div.value
   #_(->> (mapv :db/id value)
          (pr-str))
   (render-inline-links field (filter :anchor/render-inline? anchors) param-ctx)
   [:div.anchors (render-anchors (remove :anchor/render-inline? anchors) param-ctx)]])


(comment
  (let [initial-select (let [result (first (option/get-option-records field param-ctx))]
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
               ; todo assert selected value is in record set
               ; need lower level select component that can be reused here and in select.cljs
               select-options (->> (option/get-option-records field param-ctx)
                                   (mapv (fn [result]
                                           (assert (= 1 (count result)) "Cannot use multiple find-elements for an options-link")
                                           (let [entity (first result)]
                                             [(:db/id entity) (option/label-prop field result)])))
                                   (sort-by second)
                                   (map (fn [[dbid label-prop]]
                                          [:option {:key (hash dbid) :value (pr-str dbid)} label-prop])))]
           [:div.table-controls
            [:select props select-options]
            [:button {:on-click #(user-swap! {:tx (tx/edit-entity (:db/id entity) ident [] [@select-value-atom])})} "⬆"]])]))))


(defn table-many-ref-component [value field anchors props param-ctx]
  [:div.value
   #_(->> (mapv :db/id value)
          (pr-str))
   (render-inline-links field (filter :anchor/render-inline? anchors) param-ctx)
   [:div.anchors (render-anchors (remove :anchor/render-inline? anchors) param-ctx)]])


(defn multi-select-ref [value field anchors props {:keys [user-swap!] :as param-ctx}]
  (let [add-item! #(user-swap! {:tx (tx/edit-entity (:db/id (:entity param-ctx)) (-> field :field/attribute :attribute/ident) [] [nil])})]
    (multi-select* multi-select-markup value add-item! field anchors props param-ctx))) ;add-item! is: add nil to set


(defn multi-select-ref-component [value field anchors props {:keys [user-swap!] :as param-ctx}]
  (let [temp-id! (partial hc/*temp-id!* (-> (:entity param-ctx) :db/id :conn-id)) ; bound to fix render bug
        add-item! #(user-swap! {:tx (tx/edit-entity (:db/id (:entity param-ctx)) (-> field :field/attribute :attribute/ident) [] [(temp-id!)])})]
    [multi-select* multi-select-markup value add-item! field anchors props param-ctx])) ;add new entity to set


(defn code-editor [value field anchors props {:keys [user-swap!] :as param-ctx}]
  (let [ident (-> field :field/attribute :attribute/ident)
        change! #(user-swap! {:tx (tx/edit-entity (:db/id (:entity param-ctx)) ident [value] [%])})]
    ^{:key ident}
    [:div.value
     (let [props (if-not (nil? (:read-only props))
                   (-> props
                       (dissoc :read-only)
                       (assoc :readOnly (if (:read-only props)
                                          "nocursor"
                                          false)))
                   props)]
       [code-editor* value change! props])]))


(defn valid-date-str? [s]
  (or (empty? s)
      (let [ms (.parse js/Date s)]                          ; NaN if not valid string
        (integer? ms))))


(defn instant [value field anchors props {:keys [user-swap!] :as param-ctx}]
  (let [attribute (:field/attribute field)
        on-change! #(user-swap! {:tx (tx/update-entity-attr (:entity param-ctx) attribute %)})
        parse-string (fn [s]
                       (if (empty? s)
                         nil
                         (let [ms (.parse js/Date s)]
                           (js/Date. ms))))
        to-string #(some-> % .toISOString)]
    [input/validated-input value on-change! parse-string to-string valid-date-str? props]))


(defn text [value field anchors props param-ctx]
  [:div.value
   [:span.text
    (case (-> (:attribute param-ctx) :attribute/cardinality :db/ident)
      :db.cardinality/many (map pr-str value)
      (pr-str value))]
   (render-inline-links field (filter :anchor/render-inline? anchors) param-ctx)
   [:div.anchors (render-anchors (remove :anchor/render-inline? anchors) param-ctx)]])


(defn default [value field anchors props param-ctx]
  (let [{:keys [:attribute/valueType :attribute/cardinality :attribute/isComponent]} (:field/attribute field)]
    [input/input*
     (str {:valueType (:db/ident valueType)
           :cardinality (:db/ident cardinality)
           :isComponent isComponent})
     #()
     {:read-only true}]))


(defn raw [value _ anchors props param-ctx]
  (let [on-change! #((:user-swap! param-ctx) {:tx (tx/update-entity-attr (:entity param-ctx) (:attribute param-ctx) %)})]
    [input/edn-input* value on-change! props]))
