(ns hypercrud.browser.pages.entity
  (:require [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx-util]
            [hypercrud.ui.form :refer [cj-form]]))


(defn view [cur transact! graph metatype forms eid]
  "hypercrud values just get a form, with ::update and ::delete."
  (let [local-datoms (cur [:form] [])
        graph (hc/with graph @local-datoms)
        local-transact! #(swap! local-datoms tx-util/into-tx %)
        tempid! (hc/tempid!-factory)]
    [:div
     [cj-form graph eid metatype forms local-transact! tempid!]
     [:button {:on-click #(transact! @local-datoms)
               :disabled (empty? @local-datoms)}
      (if (tx-util/tempid? eid) "Create" "Update")]]))
