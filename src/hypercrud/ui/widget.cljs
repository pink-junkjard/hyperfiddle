(ns hypercrud.ui.widget
  (:require [clojure.string :as string]
            [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx-util]
            [hypercrud.form.option :as option]
            [hypercrud.js.type-util :as type-util]
            [hypercrud.ui.auto-control :refer [auto-control]]
            [hypercrud.ui.code-editor :refer [code-editor*]]
            [hypercrud.ui.form :as form]
            [hypercrud.ui.input :as input]
            [hypercrud.ui.multi-select :refer [multi-select* multi-select-markup]]
            [hypercrud.ui.radio :as radio]
            [hypercrud.ui.select :refer [select*]]
            [hypercrud.ui.textarea :refer [textarea*]]))


(defn input-keyword [entity {:keys [stage-tx!] {:keys [:ident]} :field}]
  (let [value (get entity ident)
        set-attr! #(stage-tx! (tx-util/update-entity-attr entity ident %))
        parse-string keyword
        to-string #(subs (str %) 1)
        valid? (fn [s]
                 (let [kw (keyword s)
                       int? #(integer? (js/parseInt % 10))
                       safe-first (fn [s] (if (seq s) (subs s 0 1)))]
                   (and
                     (not (int? (safe-first (name kw))))
                     (not (int? (safe-first (namespace kw)))))))]
    [input/validated-input value set-attr! parse-string to-string valid?]))


(defn input [entity {:keys [stage-tx!] {:keys [:ident]} :field}]
  (let [value (get entity ident)
        set-attr! #(stage-tx! (tx-util/update-entity-attr entity ident %))]
    [input/input* {:type "text"
                   :value value
                   :on-change set-attr!}]))


(defn textarea [entity {:keys [stage-tx!] {:keys [:ident]} :field}]
  (let [value (get entity ident)
        set-attr! #(stage-tx! (tx-util/update-entity-attr entity ident %))]
    [textarea* {:type "text"
                :value value
                :on-change set-attr!}]))


(defn radio-ref [entity widget-args]
  ;;radio* needs parameterized markup fn todo
  [radio/radio-ref* entity widget-args])


(defn select-ref [entity {:keys [expanded-cur] {:keys [:ident]} :field :as widget-args}]
  ;;select* has parameterized markup fn todo
  [select* entity (assoc widget-args :expanded-cur (expanded-cur [ident]))])


(defn select-ref-component [entity {:keys [expanded-cur forms graph stage-tx!]
                                    {:keys [:ident :options]} :field}]
  (let [value (get entity ident)]
    (form/form graph value forms (option/get-form-id options entity) expanded-cur stage-tx!)))


(defn multi-select-ref [entity {:keys [stage-tx!] {:keys [:ident]} :field :as widget-args}]
  (let [add-item! #(stage-tx! (tx-util/edit-entity (:db/id entity) ident [] [nil]))]
    (multi-select* multi-select-markup entity add-item! widget-args))) ;add-item! is: add nil to set


(defn multi-select-ref-component [entity {:keys [stage-tx!] {:keys [:ident]} :field :as widget-args}]
  (let [temp-id! hc/*temp-id!*                              ; bound to fix render bug
        add-item! #(stage-tx! (tx-util/edit-entity (:db/id entity) ident [] [(temp-id!)]))]
    [multi-select* multi-select-markup entity add-item! widget-args])) ;add new entity to set


(defn code-editor [entity {:keys [stage-tx!] {:keys [:ident]} :field}]
  (let [value (get entity ident)
        change! #(stage-tx! (tx-util/edit-entity (:db/id entity) ident %1 %2))]
    ^{:key ident}
    [code-editor* value change!]))


(defn- master-detail [entity {:keys [graph] {:keys [:ident :options]} :field :as widget-args}
                      selected-id build-child]
  (let [li (fn [eid label]
             [:li {:key eid :class (if (= eid selected-id) "selected")}
              (build-child eid label)])]
    [:div.master-detail
     [:ul (-> (map (fn [eid]
                     (let [entity (hc/entity graph eid)]
                       (li eid (get entity (option/label-prop options)))))
                   (get entity ident))
              (concat (if (option/create-new? options entity)
                        (let [eid (if (tx-util/tempid? selected-id) selected-id (hc/*temp-id!*))]
                          [(li eid "Create New")])
                        [])))]
     (let [new-args (-> widget-args
                        (assoc-in [:field :cardinality] :db.cardinality/one))]
       (if (nil? selected-id)
         [:div "Select the " (string/capitalize (name ident))]
         [auto-control (assoc entity ident selected-id) new-args]))]))


(defn master-detail-url [entity widget-args]
  (let [last-path-param (last (string/split (-> js/document .-location .-pathname) "/"))
        selected-id (type-util/string->int last-path-param)]
    (master-detail entity widget-args selected-id (fn [id label] [:a {:href (str "./" id)} label]))))


(defn master-detail-state [entity widget-args]
  (master-detail entity widget-args "todo"
                 (fn [id label]
                   [:a {:on-click #(.log js/console "todo wire up a state change")} label])))


(defn valid-date-str? [s]
  (let [ms (.parse js/Date s)]                              ; NaN if not valid string
    (integer? ms)))


(defn instant [entity {:keys [stage-tx!] {:keys [:ident]} :field}]
  (let [value (get entity ident)
        set-attr! #(stage-tx! (tx-util/update-entity-attr entity ident %))
        parse-string #(let [ms (.parse js/Date %)]
                       (js/Date. ms))
        to-string #(some-> % .toISOString)]
    [input/validated-input value set-attr! parse-string to-string valid-date-str?]))


(defn default [field]
  [input/input* {:type "text"
                 :value (str (select-keys field [:valueType :cardinality :isComponent]))
                 :read-only true}])
