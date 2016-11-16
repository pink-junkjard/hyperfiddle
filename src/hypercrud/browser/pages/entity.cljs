(ns hypercrud.browser.pages.entity
  (:require [hypercrud.browser.links :as links]
            [hypercrud.client.core :as hc]
            [hypercrud.form.q-util :as q-util]
            [hypercrud.ui.form :as form]))


(defn ui [cur stage-tx! graph entity                        ;todo this is a set of entities, a result
          find-elements navigate-cmp param-ctx
          ]
  "hypercrud values just get a form, with ::update and ::delete."
  (let [expanded-cur (cur [:expanded] {})                   ; {:community/neighborhood {:neighborhood/district {:district/region {}}}}
        dbval (-> entity .-dbgraph .-dbval)
        entity (hc/entity (hc/get-dbgraph graph dbval) (.-dbid entity))]
    [:div
     (map (fn [{:keys [:find-element/form :find-element/connection] :as find-element}]
            [form/form graph entity form expanded-cur stage-tx! navigate-cmp])
          find-elements)
     #_[:button {:on-click #(-> (transact! @local-statements)
                                (p/then (fn [{:keys [tempids]}]
                                          (if (tx/tempid? (.-dbid entity))
                                            (navigate! (str "./" (get tempids (.-dbid entity))))))))
                 :disabled (empty? @local-statements)}
        (if (tx/tempid? (.-dbid entity)) "Create" "Update")]
     #_[:button {:on-click #(-> (transact! [[:db.fn/retractEntity (.-dbid entity)]])
                                (p/then (fn [_] (navigate! (str "../../../")))))
                 :disabled (tx/tempid? (.-dbid entity))}
        "Delete"]
     [:div
      [:span "Form Links: "]
      (->> (map :find-element/form find-elements)
           (mapcat (fn [{link :form/link}]
                     (let [param-ctx (merge param-ctx
                                            {:entity entity})]
                       (links/query-link link param-ctx
                                         (fn [href]
                                           ^{:key (:link/ident link)}
                                           [navigate-cmp {:href href} (:link/prompt link)])))))
           (interpose " "))]]))


(defn query [state dbid form param-ctx]
  (form/query dbid form (get state :expanded nil) q-util/build-params-from-formula param-ctx))


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
