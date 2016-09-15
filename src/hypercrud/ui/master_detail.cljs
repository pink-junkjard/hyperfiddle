(ns hypercrud.ui.master-detail
  (:require [clojure.string :as string]
            [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx-util]
            [hypercrud.form.option :as option]
            [hypercrud.ui.auto-control :refer [auto-control]]))


(defn master-detail* [entity {:keys [graph] {:keys [:ident :options]} :field :as widget-args} selected-cur]
  (let [li (fn [eid label]
             [:li {:key eid :class (if (= eid @selected-cur) "selected")}
              [:a {:href "#" :on-click #(reset! selected-cur eid)} label]])]
    [:div.master-detail
     [:ul (doall (-> (map (fn [eid]
                            (let [entity (hc/entity graph eid)]
                              (li eid (get entity (option/label-prop options)))))
                          (get entity ident))
                     (concat (if (option/create-new? options entity)
                               ; todo this tempid shouldn't be created until after clicked
                               (let [eid (if (tx-util/tempid? @selected-cur) @selected-cur (hc/*temp-id!*))]
                                 [(li eid "Create New")])
                               []))))]
     (let [new-args (-> widget-args
                        (assoc-in [:field :cardinality] :db.cardinality/one))]
       (if (nil? @selected-cur)
         [:div "Select the " (string/capitalize (name ident))]
         [auto-control (assoc entity ident @selected-cur) new-args]))]))
