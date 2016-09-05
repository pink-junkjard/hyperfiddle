(ns hypercrud.browser.pages.entity
  (:require [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx-util]
            [hypercrud.ui.form :as form]
            [promesa.core :as p]))


(defn ui [cur transact! graph form-name forms eid navigate!]
  "hypercrud values just get a form, with ::update and ::delete."
  (let [local-statements (cur [:statements] [])
        expanded-cur (cur [:expanded] {})                   ; {:community/neighborhood {:neighborhood/district {:district/region {}}}}
        graph (hc/with graph @local-statements)
        local-transact! #(swap! local-statements tx-util/into-tx %)
        tempid! (hc/tempid!-factory)]
    [:div
     [form/form graph eid form-name forms expanded-cur local-transact! tempid!]
     [:button {:on-click #(-> (transact! @local-statements)
                              (p/then (fn [{:keys [tempids]}]
                                        (if (tx-util/tempid? eid)
                                          (navigate! (str "./" (get tempids eid)))))))
               :disabled (empty? @local-statements)}
      (if (tx-util/tempid? eid) "Create" "Update")]
     [:button {:on-click #(-> (transact! [[:db.fn/retractEntity eid]])
                              (p/then (fn [_] (navigate! (str "../../../")))))
               :disabled (tx-util/tempid? eid)}
      "Delete"]]))


(defn query [eid state form-name forms]
  (form/query eid (get state :expanded nil) (get forms form-name) forms))


(comment
  "Various strategies for form local state and features, staging areas, recursive discard"

  {:community/neighborhood {:neighborhood/district {:district/region {}}}

   :community/x {}}

  {:community/neighborhood {}}
  {:community/neighborhood nil}

  {:db/id 17592186045440
   :community/neighborhood {:db/id 17592186045439
                            :neighborhood/district {:db/id 17592186045438
                                                    :district/name "asdf"}}
   :community/orgtype #{{} {:community-orgtpe/blog {}}}}


  {:statements []
   :form-state {:neighborhood/name {:statements []
                                    :form-state {}}}}


  {:statements []
   :form-state {:neighborhood/name {:statements []
                                    :form-state {}}
                :community/category {:statements []
                                     :form-state {}}}}

  {:neighborhood/name {:form-state {:district/name {:editing true}}}})
