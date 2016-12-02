(ns hypercrud.browser.pages.field
  (:require [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx]
            [hypercrud.form.q-util :as q-util]
            [hypercrud.ui.auto-control :refer [auto-control]]
            [hypercrud.ui.form :as form]))


(defn ui [stage-tx! graph entity field navigate-cmp]
  (let [entity (hc/entity (hc/get-dbgraph graph (-> entity .-dbgraph .-dbval)) (.-dbid entity))]
    [:div
     [auto-control entity {:field field
                           :graph graph
                           :navigate-cmp navigate-cmp
                           :stage-tx! stage-tx!}]]))


(defn query [dbid field param-ctx]
  (let [param-ctx (merge param-ctx {:id (.-id dbid)})
        dbval (get param-ctx :dbval)
        p-filler q-util/build-params-from-formula
        option-queries (form/field-queries p-filler param-ctx field)]
    (if (not (tx/tempid? dbid))
      (merge option-queries
             {dbid ['[:find ?e :in $ ?e :where [?e]]
                    {"$" dbval "?e" dbid}
                    {"?e" [dbval [:db/id (-> field :field/attribute :attribute/ident)]]}]})
      option-queries)))
