(ns hypercrud.ui.select
  (:require [hypercrud.client.core :as hc]
            [hypercrud.ui.form :as form]
            [reagent.core :as reagent]
            [hypercrud.client.tx :as tx-util]
            [hypercrud.client.util :as util]))


(defn select-option
  ":option :value and :on-change is a string, since that's how the dom works"
  [client label-prop eid]
  (if (keyword? eid)
    [:option {:key eid :value (util/transit-encode eid)} (name eid)]
    [:option {:key eid :value (util/transit-encode eid)}
     (name (get (hc/entity client eid) label-prop))]))


(defn select* [client forms options value change! transact!]
  (let [updating? (reagent/atom false)]
    (fn [client forms {:keys [label-prop form query]} value change! transact!]
      (let [eids (hc/select client [query])
            props {;; normalize value for the dom - value is either nil, an :ident (keyword), or eid
                   :value (util/transit-encode
                            (cond
                              (nil? value) nil
                              (tx-util/tempid? value) "create-new"
                              :else value))

                   ;; reconstruct the typed value (keyword or cj item ref)
                   :on-change #(do
                                (let [select-value (util/transit-decode (.-target.value %))
                                      eid (cond
                                            (nil? select-value) nil
                                            (= "create-new" select-value) (hc/tempid! client)
                                            :else-hc-select-option-node select-value)]
                                  (change! [:db/retract value]
                                           [:db/add eid])
                                  ;; and also add our new guy to the option list (for all combos)
                                  (reset! updating? false)))}

            create-new? (some-> value tx-util/tempid?)
            show-form? (or @updating? create-new?)]

        [:div.editable-select {:key (hash eids)}
         (if form
           [:button {:on-click #(swap! updating? not) :disabled (= nil value)} (if show-form? "Discard" "Edit")])
         [:span
          [:select props (-> (doall (map (fn [eid]
                                           (select-option client label-prop eid))
                                         eids))
                             (concat
                               (if (not form)
                                 []                         ;can't create-new
                                 [[:option {:key :create-new :value (util/transit-encode "create-new")} "Create New"]])
                               [[:option {:key :blank :value (util/transit-encode nil)} "--"]]))]]
         (if show-form?
           ;; TODO branch the client and provide datom for :meta/type in create-new case
           [form/cj-form client value forms transact!])]))))
