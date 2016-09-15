(ns hypercrud.ui.master-detail
  (:require [clojure.string :as string]
            [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx-util]
            [hypercrud.form.option :as option]
            [hypercrud.js.type-util :as type-util]
            [hypercrud.ui.auto-control :refer [auto-control]]))


(defn master-detail* [entity {:keys [graph] {:keys [:ident :options]} :field :as widget-args}
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


; cannot support multiple master-detail-urls on a page
(defn master-detail-url [entity widget-args]
  (let [last-path-param (last (string/split (-> js/document .-location .-pathname) "/"))
        selected-id (type-util/string->int last-path-param)]
    (master-detail* entity widget-args selected-id (fn [id label] [:a {:href (str "./" id)} label]))))
