(ns hypercrud.ui.form
  (:require [hypercrud.ui.auto-control :refer [auto-control]]
            [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx-util]
            [hypercrud.form.util :as form-util]))

(defn field [entity {{:keys [:prompt]} :field :as widget-args}]
  [:div.field
   (if prompt [:label prompt])
   [auto-control entity widget-args]])


(defn form [graph eid forms form-id expanded-cur stage-tx! navigate-cmp]
  (let [entity (hc/entity graph eid)]
    [:div.form
     (map (fn [{:keys [:ident] :as fieldinfo}]
            ^{:key ident}
            [field entity {:expanded-cur (expanded-cur [ident])
                           :field fieldinfo
                           :forms forms
                           :graph graph
                           :navigate-cmp navigate-cmp
                           :stage-tx! stage-tx!}])
          (get forms form-id))]))


(defn query [eid forms form-id expanded-forms]
  (let [q-and-params (if (not (tx-util/tempid? eid))
                       {:query '[:find [?eid ...] :in $ ?eid :where [?eid]]
                        :params [eid]})]
    (form-util/query forms form-id expanded-forms q-and-params)))
