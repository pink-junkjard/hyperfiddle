(ns hypercrud.browser.pages.field
  (:require [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx-util]
            [hypercrud.form.q-util :as q-util]
            [hypercrud.types :refer [->Entity]]
            [hypercrud.ui.form :as form]
            [hypercrud.ui.auto-control :refer [auto-control]]
            [hypercrud.util :as util]))


(defn ui [cur transact! graph entity forms queries form-id field-ident navigate-cmp]
  (let [form (get forms form-id)
        field (first (filter #(= (:ident %) field-ident) (:form/field form)))
        local-statements (cur [:statements] [])
        graph (hc/with graph (-> entity .-dbval .-dbval) @local-statements)
        entity (->Entity (.-dbid entity) (hc/get-dbgraph graph (-> entity .-dbval .-dbval)))
        stage-tx! #(swap! local-statements tx-util/into-tx %)
        expanded-cur (cur [:expanded (:ident field)]
                          ; hacky but we currently only want expanded edit forms where we draw tables
                          (if (= :db.cardinality/many (:cardinality field)) {} nil))]
    [:div
     [auto-control entity {:expanded-cur expanded-cur
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
(defn query [state dbid forms queries form-id field-ident param-ctx]
  (let [update-form-field (fn [fields] (filter #(= field-ident (:ident %)) fields))
        form (util/update-existing (get forms form-id) :form/field update-form-field)]
    (form/query dbid forms queries form (get state :expanded nil) q-util/build-params-from-formula param-ctx)))