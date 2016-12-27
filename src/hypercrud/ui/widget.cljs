(ns hypercrud.ui.widget
  (:require [cljs.reader :as reader]
            [hypercrud.browser.links :as links]
            [hypercrud.browser.core :as browser]
            [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx]
            [hypercrud.form.option :as option]
            [hypercrud.ui.auto-control :refer [auto-control]]
            [hypercrud.ui.code-editor :refer [code-editor*]]
            [hypercrud.ui.input :as input]
            [hypercrud.ui.multi-select :refer [multi-select* multi-select-markup]]
            [hypercrud.ui.radio :as radio]
            [hypercrud.ui.select :refer [select*]]
            [hypercrud.ui.textarea :refer [textarea*]]
            [reagent.core :as r]))


(defn link-thing [link-ctxs param-ctx]
  [:div.links
   (->> link-ctxs
        (remove :link-ctx/render-inline?)
        (filter #(links/link-visible? % param-ctx))
        (map (fn [{:keys [:link-ctx/link] :as link-ctx}]
               ^{:key (:db/id link-ctx)}
               [(:navigate-cmp param-ctx) (links/build-link-props link-ctx param-ctx) (:link/prompt link) param-ctx]))
        (interpose " · "))])


(defn render-inline-links [field link-ctxs param-ctx]
  (let [field-dbid (.-dbid field)
        ; todo do we need a different param-ctx for rendering the ui?
        param-ctx (assoc param-ctx
                    :isComponent (-> field :field/attribute :attribute/isComponent)
                    :debug (str "table-many-ref:" field-dbid ":" (:field/prompt field)))]
    (->> link-ctxs
         (filter :link-ctx/render-inline?)
         (filter #(links/link-visible? % param-ctx))
         (map (fn [link-ctx]
                ^{:key (-> link-ctx :db/id)}
                [browser/ui (links/build-params-map link-ctx param-ctx) param-ctx])))))


(defn input-keyword [entity field link-ctxs props {:keys [user-swap!] :as param-ctx}]
  (let [{:keys [:attribute/ident] :as attribute} (:field/attribute field)
        value (get entity ident)
        on-change! #(user-swap! {:tx (tx/update-entity-attr entity attribute %)})
        parse-string reader/read-string
        to-string str
        valid? #(try (let [code (reader/read-string %)]
                       (or (nil? code) (keyword? code)))
                     (catch :default e false))]
    [input/validated-input value on-change! parse-string to-string valid?]))


(defn input [entity field link-ctxs props {:keys [user-swap!] :as param-ctx}]
  (let [{:keys [:attribute/ident] :as attribute} (:field/attribute field)
        value (get entity ident)
        on-change! #(user-swap! {:tx (tx/update-entity-attr entity attribute %)})]
    [input/input* value on-change! props]))


(defn input-long [entity field link-ctxs props {:keys [user-swap!] :as param-ctx}]
  (let [{:keys [:attribute/ident] :as attribute} (:field/attribute field)]
    [input/validated-input
     (get entity ident) #(user-swap! {:tx (tx/update-entity-attr entity attribute %)})
     #(js/parseInt % 10) pr-str
     #(integer? (js/parseInt % 10))]))


(defn textarea [entity field link-ctxs props {:keys [user-swap!] :as param-ctx}]
  (let [{:keys [:attribute/ident] :as attribute} (:field/attribute field)
        value (get entity ident)
        set-attr! #(user-swap! {:tx (tx/update-entity-attr entity attribute %)})]
    [textarea* {:type "text"
                :value value
                :on-change set-attr!}]))


(defn radio-ref [entity field link-ctxs props param-ctx]
  ;;radio* needs parameterized markup fn todo
  [radio/radio-ref* entity field param-ctx])


; this can be used sometimes, on the entity page, but not the query page
(defn select-ref [entity field link-ctxs props param-ctx]
  [:div.value
   [:div.editable-select {:key (option/get-key field)}
    (link-thing link-ctxs param-ctx)
    [:span.select
     (select* entity field param-ctx)]]
   (render-inline-links field link-ctxs param-ctx)])


(defn select-ref-component [entity field link-ctxs props param-ctx]
  [:div.value
   #_(pr-str (get-in entity [(-> field :field/attribute :attribute/ident) :db/id]))
   (render-inline-links field link-ctxs param-ctx)
   (link-thing link-ctxs param-ctx)])


(defn table-many-ref [entity field link-ctxs props param-ctx]
  [:div.value
   #_(->> (get entity (-> field :field/attribute :attribute/ident))
          (mapv :db/id)
          (pr-str))
   (render-inline-links field link-ctxs param-ctx)
   (link-thing link-ctxs param-ctx)])


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


(defn table-many-ref-component [entity field link-ctxs props param-ctx]
  [:div.value
   #_(->> (get entity (-> field :field/attribute :attribute/ident))
          (mapv :db/id)
          (pr-str))
   (render-inline-links field link-ctxs param-ctx)
   (link-thing link-ctxs param-ctx)])


(defn multi-select-ref [entity field link-ctxs props {:keys [user-swap!] :as param-ctx}]
  (let [add-item! #(user-swap! {:tx (tx/edit-entity (:db/id entity) (-> field :field/attribute :attribute/ident) [] [nil])})]
    (multi-select* multi-select-markup entity add-item! field link-ctxs props param-ctx))) ;add-item! is: add nil to set


(defn multi-select-ref-component [entity field link-ctxs props {:keys [user-swap!] :as param-ctx}]
  (let [temp-id! (partial hc/*temp-id!* (-> entity .-dbgraph .-dbval :conn-id)) ; bound to fix render bug
        add-item! #(user-swap! {:tx (tx/edit-entity (:db/id entity) (-> field :field/attribute :attribute/ident) [] [(temp-id!)])})]
    [multi-select* multi-select-markup entity add-item! field link-ctxs props param-ctx])) ;add new entity to set


(defn code-editor [entity field link-ctxs props {:keys [user-swap!] :as param-ctx}]
  (let [ident (-> field :field/attribute :attribute/ident)
        value (get entity ident)
        change! #(user-swap! {:tx (tx/edit-entity (:db/id entity) ident [value] [%])})]
    ^{:key ident}
    [:div.value
     [code-editor* value change!]]))


(defn valid-date-str? [s]
  (or (empty? s)
      (let [ms (.parse js/Date s)]                          ; NaN if not valid string
        (integer? ms))))


(defn instant [entity field link-ctxs props {:keys [user-swap!] :as param-ctx}]
  (let [{:keys [:attribute/ident] :as attribute} (:field/attribute field)
        value (get entity ident)
        on-change! #(user-swap! {:tx (tx/update-entity-attr entity attribute %)})
        parse-string (fn [s]
                       (if (empty? s)
                         nil
                         (let [ms (.parse js/Date s)]
                           (js/Date. ms))))
        to-string #(some-> % .toISOString)]
    [input/validated-input value on-change! parse-string to-string valid-date-str?]))


(defn text [entity field link-ctxs props param-ctx]
  [:div.value
   (let [value (get entity (-> field :field/attribute :attribute/ident))]
     (condp = (-> field :field/attribute :attribute/cardinality :db/ident)
       :db.cardinality/one (pr-str value)
       :db.cardinality/many (map pr-str value)))
   (render-inline-links field link-ctxs param-ctx)
   (link-thing link-ctxs param-ctx)])


(defn default [entity field link-ctxs props param-ctx]
  (let [{:keys [:attribute/valueType :attribute/cardinality :attribute/isComponent]} (:field/attribute field)]
    [input/input*
     (str {:valueType (:db/ident valueType)
           :cardinality (:db/ident cardinality)
           :isComponent isComponent})
     #()
     {:read-only true}]))
