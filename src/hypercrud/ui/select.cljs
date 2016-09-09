(ns hypercrud.ui.select
  (:require [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx-util]
            [hypercrud.ui.form :as form]))


(defn select-option
  ":option :value and :on-change is a string, since that's how the dom works"
  [graph label-prop eid]
  (if (keyword? eid)
    [:option {:value (str eid)} (name eid)]
    [:option {:value (str eid)}
     (str (get (hc/entity graph eid) label-prop))]))


(defn select* [graph forms {:keys [:option/label-prop :option/form] {[query args] :query/value} :option/query} value expanded-cur
               change! transact! tempid!]
  (let [option-eids (hc/select graph (hash query) query)
        props {;; normalize value for the dom - value is either nil, an :ident (keyword), or eid
               :value (cond
                        (nil? value) ""
                        (tx-util/tempid? value) "create-new"
                        :else (str value))

               ;; reconstruct the typed value
               :on-change #(do
                            (let [select-value (.-target.value %)
                                  eid (cond
                                        (= "" select-value) nil
                                        (= "create-new" select-value) (tempid!)
                                        :else-hc-select-option-node (js/parseInt select-value 10))]
                              ;reset the cursor before change! otherwise npe when trying to render
                              ;todo these both set the same cursor, and should be atomic
                              (reset! expanded-cur (if (= "create-new" select-value) {} nil))
                              (change! [value] [eid])
                              ;; and also add our new guy to the option list (for all combos)
                              ))}
        create-new? (some-> value tx-util/tempid?)
        show-form? (or (not= nil @expanded-cur) create-new?)]

    [:div.editable-select {:key (hash option-eids)}
     (if (and form (not show-form?))
       [:button {:on-click #(reset! expanded-cur {})
                 :disabled (= nil value)} "Edit"])
     [:span
      [:select props (-> (->> (mapv #(hc/entity graph %) option-eids)
                              (sort-by #(get % label-prop))
                              (mapv (fn [entity]
                                      (let [eid (:db/id entity)]
                                        ^{:key eid}
                                        [select-option graph label-prop eid]))))
                         (concat
                           (if (not form)
                             []                             ;can't create-new
                             [[:option {:key :create-new :value "create-new"} "Create New"]])
                           [[:option {:key :blank :value ""} "--"]]))]]
     (if show-form?
       ;; TODO branch the client in create-new case
       [form/form graph value forms form expanded-cur transact! tempid!])]))
