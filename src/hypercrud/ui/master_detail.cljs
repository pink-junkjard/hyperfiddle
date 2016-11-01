(ns hypercrud.ui.master-detail
  (:require [clojure.string :as string]
            [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx-util]
            [hypercrud.form.option :as option]
            [hypercrud.types :refer [->DbId]]
            [hypercrud.ui.auto-control :refer [auto-control]]
            [hypercrud.ui.form :as form]))


(defn master-detail* [entity selected-dbid
                      {:keys [field graph navigate-cmp stage-tx!] :as widget-args}
                      & [filter-entities detail-renderer]]
  (let [ident (-> field :field/attribute :attribute/ident)
        options (option/gimme-useful-options field)
        detail-renderer (or detail-renderer form/form)
        temp-id! (partial hc/*temp-id!* (-> entity .-dbgraph .-dbval .-conn-id))
        li (fn [dbid label is-selected? & retract]
             [:li {:key (hash dbid) :class (if is-selected? "selected")}
              retract
              ; todo do we need to support entities across databases?
              [navigate-cmp {:href (.-id dbid)} label]])]
    [:div.master-detail
     [:ul (doall (-> (->> (get entity ident)
                          (filter (or filter-entities (constantly true)))
                          (map (fn [child-entity]
                                 (let [dbid (:db/id child-entity)]
                                   (li dbid
                                       (get child-entity (option/label-prop options))
                                       (= dbid selected-dbid)
                                       [:button.retract-detail
                                        {:key "retract"
                                         ; todo if im selected what do
                                         :on-click #(stage-tx! (tx-util/edit-entity (:db/id entity) ident [dbid] []))} "⌦"])))))
                     (concat (if (option/create-new? options entity)
                               (let [dbid (temp-id!)]
                                 [(li dbid "Create New" false)])
                               []))))]
     (if (nil? selected-dbid)
       [:span "Select the " (string/capitalize (name ident))]
       (let [selected-entity (hc/entity (.-dbgraph entity) selected-dbid)]
         ^{:key (hash selected-dbid)}
         [detail-renderer graph selected-entity
          (option/get-form options entity)
          ((:expanded-cur widget-args) [(:db/id entity)])
          (:stage-tx! widget-args)
          (:navigate-cmp widget-args)]))]))
