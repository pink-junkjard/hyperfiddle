(ns hypercrud.ui.select
  (:require [hypercrud.client.core :as hypercrud]
            [hypercrud.client.reagent :as hcr]
            [hypercrud.ui.form :as form]
            [reagent.core :as reagent]
            [hypercrud.client.tx-util :as tx-util]
            [hypercrud.client.util :as util]))


(defn select-option
  ":option :value and :on-change is a string, since that's how the dom works"
  [client label-prop eid]
  (if (keyword? eid)
    [:option {:key eid :value (util/transit-encode eid)} (name eid)]
    ^{:key [eid (hypercrud/tx client)]}
    [hcr/entity client eid
     (fn [entity]
       [:option {:key eid :value (util/transit-encode eid)}
        (name (get entity label-prop))])
     (fn [entity] [:option])]))


(defn select* [client forms options value change! transact!]
  (let [updating? (reagent/atom false)]
    (fn [client forms {:keys [label-prop typetag query]} value change! transact!]
      [hcr/query client query
       (fn [eids]
         (let [option-form (:meta/type (first eids))
               kw-ref? (every? :readonly option-form)
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
                                               (= "create-new" select-value) (hypercrud/tempid! client)
                                               :else-hc-select-option-node select-value)]
                                     (change! [:db/retract value]
                                              [:db/add eid])
                                     ;; and also add our new guy to the option list (for all combos)
                                     (reset! updating? false)))}

               create-new? (some-> value tx-util/tempid?)
               show-form? (or @updating? create-new?)]

           ^{:key (hash eids)}
           [:div.editable-select
            (if (not kw-ref?)
              [:button {:on-click #(swap! updating? not) :disabled (= nil value)} (if show-form? "Discard" "Edit")])
            [:span
             [:select props (-> (doall (map (fn [eid]
                                              (select-option client label-prop eid))
                                            eids))
                                (concat
                                  (if kw-ref?
                                    []                      ;can't create-new
                                    [[:option {:key :create-new :value (util/transit-encode "create-new")} "Create New"]])
                                  [[:option {:key :blank :value (util/transit-encode nil)} "--"]]))]]
            (if show-form?
              [form/cj-form client value forms transact!])]))])))
