(ns hypercrud.ui.master-detail
  (:require [clojure.string :as string]
            [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx-util]
            [hypercrud.form.option :as option]
            [hypercrud.ui.auto-control :refer [auto-control]]
            [hypercrud.types :as types]))


(defn master-detail* [entity {:keys [graph stage-tx!] {:keys [ident options]} :field :as widget-args} selected-cur & [filter-entities detail-renderer]]
  (let [detail-renderer (or detail-renderer auto-control)
        dbval (-> entity meta :dbval)
        temp-id! (partial hc/*temp-id!* (:conn-id dbval))
        li (fn [key label is-selected? on-click & retract]
             [:li {:key key :class (if is-selected? "selected")}
              retract
              ; todo should use navigate-cmp?
              [:a {:href "#" :on-click on-click} label]])]
    [:div.master-detail
     [:ul (doall (-> (->> (get entity ident)
                          (map #(hc/entity graph dbval %))
                          (map (fn [dbid]
                                 (hc/entity graph (get graph (:conn-id dbid)) dbid)
                                 (hc/entity graph (types/->DbVal (:conn-id dbid) (:db/t entity)) dbid)))
                          (filter (or filter-entities (constantly true)))
                          (map (fn [child-entity]
                                 (let [dbid (:db/id child-entity)]
                                   (li dbid
                                       (get child-entity (option/label-prop options))
                                       (= dbid @selected-cur)
                                       #(reset! selected-cur dbid)
                                       [:button.retract-detail
                                        {:key "retract"
                                         :on-click #((stage-tx! (tx-util/edit-entity (:db/id entity) ident [dbid] []))
                                                     (reset! selected-cur nil))} "⌦"])))))
                     (concat (if (option/create-new? options entity)
                               [(li "create-new" "Create New" false
                                    #(let [dbid (temp-id!)]
                                      (stage-tx! (tx-util/edit-entity (:db/id entity) ident [] [dbid]))
                                      (reset! selected-cur dbid)))]
                               []))))]
     (let [new-args (-> widget-args
                        (update :expanded-cur #(% [(:db/id entity)]))
                        (assoc-in [:field :cardinality] :db.cardinality/one))]
       (if (nil? @selected-cur)
         [:span "Select the " (string/capitalize (name ident))]
         ^{:key @selected-cur}
         [detail-renderer (assoc entity ident @selected-cur) new-args]))]))
