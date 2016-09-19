(ns hypercrud.ui.master-detail
  (:require [clojure.string :as string]
            [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx-util]
            [hypercrud.form.option :as option]
            [hypercrud.ui.auto-control :refer [auto-control]]))


(defn master-detail* [entity {:keys [graph stage-tx!] {:keys [:ident :options]} :field :as widget-args} selected-cur]
  (let [temp-id! hc/*temp-id!*
        li (fn [key label is-selected? on-click]
             [:li {:key key :class (if is-selected? "selected")}
              ; todo should use navigate-cmp?
              [:a {:href "#" :on-click on-click} label]])]
    [:div.master-detail
     [:ul (doall (-> (map (fn [eid]
                            (let [entity (hc/entity graph eid)]
                              (li eid
                                  (get entity (option/label-prop options))
                                  (= eid @selected-cur)
                                  #(reset! selected-cur eid))))
                          (get entity ident))
                     (concat (if (option/create-new? options entity)
                               [(li "create-new" "Create New" false
                                    #(let [eid (temp-id!)]
                                      (stage-tx! (tx-util/edit-entity (:db/id entity) ident [] [eid]))
                                      (reset! selected-cur eid)))]
                               []))))]
     (let [new-args (-> widget-args
                        (update :expanded-cur #(% [(:db/id entity)]))
                        (assoc-in [:field :cardinality] :db.cardinality/one))]
       (if (nil? @selected-cur)
         [:div "Select the " (string/capitalize (name ident))]
         [auto-control (assoc entity ident @selected-cur) new-args]))]))
