(ns hypercrud.browser.pages.entity
  (:require [hypercrud.browser.links :as links]
            [hypercrud.client.core :as hc]
            [hypercrud.form.q-util :as q-util]
            [hypercrud.ui.form :as form]))


(defn ui [cur stage-tx! graph result ordered-forms navigate-cmp param-ctx]
  (let [expanded-cur (cur [:expanded] {})]                  ; {:community/neighborhood {:neighborhood/district {:district/region {}}}}
    [:div
     (map (fn [entity form]
            ^{:key (hash [(.-dbid entity) (.-dbid form)])}
            [form/form graph entity form expanded-cur stage-tx! navigate-cmp])
          result ordered-forms)
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
      (->> ordered-forms
           (mapcat :form/link)
           (mapv (fn [link]
                   (let [param-ctx (merge param-ctx
                                          {:result result})]
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
