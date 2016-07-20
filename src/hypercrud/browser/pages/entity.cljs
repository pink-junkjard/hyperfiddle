(ns hypercrud.browser.pages.entity
  (:require [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx-util]
            [hypercrud.ui.form :as form]))


(defn ui [cur transact! graph metatype forms eid]
  "hypercrud values just get a form, with ::update and ::delete."
  (let [local-statements (cur [:statements] [])
        expanded-cur (cur [:expanded] {})                   ; {:community/neighborhood {:neighborhood/district {:district/region {}}}}
        graph (hc/with graph @local-statements)
        local-transact! #(swap! local-statements tx-util/into-tx %)
        tempid! (hc/tempid!-factory)]
    [:div
     [form/form graph eid metatype forms expanded-cur local-transact! tempid!]
     [:button {:on-click #(transact! @local-statements)
               :disabled (empty? @local-statements)}
      (if (tx-util/tempid? eid) "Create" "Update")]]))


(defn query [eid state metatype forms]
  (form/query eid (get state :expanded nil) (metatype forms) forms))


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
