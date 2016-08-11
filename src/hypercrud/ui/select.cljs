(ns hypercrud.ui.select
  (:require [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx-util]
            [hypercrud.client.internal :as util]
            [hypercrud.ui.form :as form]))


(defn select-option
  ":option :value and :on-change is a string, since that's how the dom works"
  [graph label-prop eid]
  (if (keyword? eid)
    [:option {:value (util/transit-encode eid)} (name eid)]
    [:option {:value (util/transit-encode eid)}
     (name (get (hc/entity graph eid) label-prop))]))


(defn select* [graph forms {:keys [label-prop metatype form query]} value expanded-cur
               change! transact! tempid!]
  (let [option-eids (hc/select graph (hash query))
        props {;; normalize value for the dom - value is either nil, an :ident (keyword), or eid
               :value (util/transit-encode
                        (cond
                          (nil? value) nil
                          (tx-util/tempid? value) "create-new"
                          :else value))

               ;; reconstruct the typed value
               :on-change #(do
                            (let [select-value (util/transit-decode (.-target.value %))
                                  eid (cond
                                        (nil? select-value) nil
                                        (= "create-new" select-value) (tempid!)
                                        :else-hc-select-option-node select-value)]
                              ;reset the cursor before change! otherwise npe when trying to render
                              ;todo these both set the same cursor, and should be atomic
                              (reset! expanded-cur (if (= "create-new" select-value) {} nil))
                              (change! [:db/retract value]
                                       [:db/add eid])
                              ;; and also add our new guy to the option list (for all combos)
                              ))}
        create-new? (some-> value tx-util/tempid?)
        show-form? (or (not= nil @expanded-cur) create-new?)]

    [:div.editable-select {:key (hash option-eids)}
     (if (and form (not show-form?))
       [:button {:on-click #(reset! expanded-cur {})
                 :disabled (= nil value)} "Edit"])
     [:span
      [:select props (-> (doall (->> (map #(hc/entity graph %) option-eids)
                                     (sort-by #(get % label-prop))
                                     (map (fn [entity]
                                            (let [eid (:db/id entity)]
                                              ^{:key eid}
                                              [select-option graph label-prop eid])))))
                         (concat
                           (if (not form)
                             []                             ;can't create-new
                             [[:option {:key :create-new :value (util/transit-encode "create-new")} "Create New"]])
                           [[:option {:key :blank :value (util/transit-encode nil)} "--"]]))]]
     (if show-form?
       ;; TODO branch the client in create-new case
       [form/form graph value metatype forms expanded-cur transact! tempid!])]))
