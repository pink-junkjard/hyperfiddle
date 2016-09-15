(ns hypercrud.browser.pages.field
  (:require [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx-util]
            [hypercrud.ui.form :as form]))


(defn ui [field cur transact! graph eid forms]
  (.log js/console (pr-str field))
  (let [local-statements (cur [:statements] [])
        expanded-cur (cur [:expanded] {})
        graph (hc/with graph @local-statements)
        stage-tx! #(swap! local-statements tx-util/into-tx %)]
    [:div
     [form/field field graph (hc/entity graph eid) forms expanded-cur stage-tx!]
     [:button {:on-click #(transact! @local-statements)
               :disabled (empty? @local-statements)}
      "Update"]]))


;todo copied from entity
(defn query [state eid forms form-id]
  (form/query eid forms form-id (get state :expanded nil)))