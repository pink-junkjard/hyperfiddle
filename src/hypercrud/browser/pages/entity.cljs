(ns hypercrud.browser.pages.entity
  (:require [hypercrud.browser.links :as links]
            [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx-util]
            [hypercrud.form.q-util :as q-util]
            [hypercrud.ui.form :as form]
            [promesa.core :as p]))


(defn ui [cur transact! graph dbval dbid forms queries form-id navigate! navigate-cmp param-ctx]
  "hypercrud values just get a form, with ::update and ::delete."
  (let [local-statements (cur [:statements] [])
        expanded-cur (cur [:expanded] {})                   ; {:community/neighborhood {:neighborhood/district {:district/region {}}}}
        graph (hc/with graph dbval @local-statements)
        stage-tx! #(swap! local-statements tx-util/into-tx %)]
    [:div
     [form/form graph dbval dbid forms queries form-id expanded-cur stage-tx! navigate-cmp]
     [:button {:on-click #(-> (transact! @local-statements)
                              (p/then (fn [{:keys [tempids]}]
                                        (if (tx-util/tempid? dbid)
                                          (navigate! (str "./" (get tempids dbid)))))))
               :disabled (empty? @local-statements)}
      (if (tx-util/tempid? dbid) "Create" "Update")]
     [:button {:on-click #(-> (transact! [[:db.fn/retractEntity dbid]])
                              (p/then (fn [_] (navigate! (str "../../../")))))
               :disabled (tx-util/tempid? dbid)}
      "Delete"]
     [:div
      [:span "Links: "]
      (->> (:form/link (get forms form-id))
           (map (fn [link]
                  (let [param-ctx (merge param-ctx
                                         {:entity (hc/entity graph dbval dbid)})
                        href (links/query-link link queries param-ctx)]
                    ^{:key (:link/ident link)}
                    [navigate-cmp {:href href} (:link/prompt link)])))
           (interpose " "))]]))


(defn query [state dbid forms queries form-id param-ctx]
  (form/query dbid forms queries (get forms form-id) (get state :expanded nil) q-util/build-params-from-formula param-ctx))


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
