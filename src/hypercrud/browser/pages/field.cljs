(ns hypercrud.browser.pages.field
  (:require [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx-util]
            [hypercrud.ui.form :as form]
            [hypercrud.ui.auto-control :refer [auto-control]]))


(defn ui [field cur transact! graph eid forms]
  (let [local-statements (cur [:statements] [])
        graph (hc/with graph @local-statements)
        stage-tx! #(swap! local-statements tx-util/into-tx %)]
    [:div
     [auto-control (hc/entity graph eid) {:expanded-cur (cur [:expanded (:ident field)] {})
                                          :field field
                                          :forms forms
                                          :graph graph
                                          :stage-tx! stage-tx!}]
     [:button {:on-click #(transact! @local-statements)
               :disabled (empty? @local-statements)}
      "Update"]]))


;todo copied from entity
(defn query [state eid forms form-id]
  (form/query eid forms form-id (get state :expanded nil)))