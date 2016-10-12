(ns hypercrud.ui.form
  (:require [hypercrud.ui.auto-control :refer [auto-control]]
            [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx-util]
            [hypercrud.form.util :as form-util]))


(defn field [entity {{:keys [:prompt]} :field :as widget-args}]
  [:div.field
   (if prompt [:label prompt])
   [auto-control entity widget-args]])


; some people just want to watch the world burn
(defn form2 [graph entity forms queries form expanded-cur stage-tx! navigate-cmp]
  [:div.form
   (map (fn [{:keys [:ident] :as fieldinfo}]
          ^{:key ident}
          [field entity {:expanded-cur (expanded-cur [ident])
                         :field fieldinfo
                         :forms forms
                         :graph graph
                         :navigate-cmp navigate-cmp
                         :queries queries
                         :stage-tx! stage-tx!}])
        (:form/field form))])


(defn form [graph eid forms queries form-id expanded-cur stage-tx! navigate-cmp]
  [form2 graph (hc/entity graph eid) forms queries (get forms form-id) expanded-cur stage-tx! navigate-cmp])


(defn query [eid forms queries form expanded-forms p-filler param-ctx]
  (let [param-ctx (merge param-ctx {:id eid})]
    (if (not (tx-util/tempid? eid))
      (merge (form-util/form-option-queries forms queries form expanded-forms p-filler param-ctx)
             (let [q '[:find [?e ...] :in $ ?e :where [?e]]
                   pull-exp (form-util/form-pull-exp forms form expanded-forms)]
               {eid [q [eid] pull-exp]}))
      (form-util/form-option-queries forms queries form expanded-forms p-filler param-ctx))))
