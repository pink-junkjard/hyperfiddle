(ns hypercrud.browser.pages.field
  (:require [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx-util]
            [hypercrud.ui.form :as form]
            [hypercrud.ui.auto-control :refer [auto-control]]))


(defn ui [cur transact! graph eid forms queries form-id field-ident navigate-cmp]
  (let [form (get forms form-id)
        field (first (filter #(= (:ident %) field-ident) form))
        local-statements (cur [:statements] [])
        graph (hc/with graph @local-statements)
        stage-tx! #(swap! local-statements tx-util/into-tx %)
        expanded-cur (cur [:expanded (:ident field)]
                          ; hacky but we currently only want expanded edit forms where we draw tables
                          (if (= :db.cardinality/many (:cardinality field)) {} nil))]
    [:div
     [auto-control (hc/entity graph eid) {:expanded-cur expanded-cur
                                          :field field
                                          :forms forms
                                          :graph graph
                                          :navigate-cmp navigate-cmp
                                          :queries queries
                                          :stage-tx! stage-tx!}]
     [:button {:on-click #(transact! @local-statements)
               :disabled (empty? @local-statements)}
      "Update"]]))


;todo copied from entity
(defn query [state eid forms queries form-id field-ident param-ctx]
  (let [form (->> (get forms form-id)
                  (filter #(= field-ident (:ident %))))]
    (form/query eid forms queries form (get state :expanded nil) param-ctx)))