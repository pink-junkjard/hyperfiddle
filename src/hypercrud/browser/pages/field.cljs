(ns hypercrud.browser.pages.field
  (:require [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx]
            [hypercrud.form.q-util :as q-util]
            [hypercrud.form.util :as form-util]
            [hypercrud.ui.auto-control :refer [auto-control]]))


(defn ui [cur stage-tx! graph entity field navigate-cmp]
  (let [entity (hc/entity (hc/get-dbgraph graph (-> entity .-dbgraph .-dbval)) (.-dbid entity))
        expanded-cur (cur [:expanded (-> field :field/attribute :attribute/ident)]
                          ; hacky but we currently only want expanded edit forms where we draw tables
                          (if (= :db.cardinality/many (:cardinality field)) {} nil))]
    [:div
     [auto-control entity {:expanded-cur expanded-cur
                           :field field
                           :graph graph
                           :navigate-cmp navigate-cmp
                           :stage-tx! stage-tx!}]]))


(defn query [state dbid field param-ctx]
  (let [param-ctx (merge param-ctx {:id (.-id dbid)})
        dbval (get param-ctx :dbval)
        expanded-forms (get state :expanded nil)
        p-filler q-util/build-params-from-formula
        option-queries (form-util/field-queries expanded-forms p-filler param-ctx field form-util/recurse?)]
    (if (not (tx/tempid? dbid))
      (merge option-queries
             {dbid ['[:find ?e :in $ ?e :where [?e]]
                    {"$" dbval "?e" (.-id dbid)}
                    {"?e" [dbval [:db/id (-> field :field/attribute :attribute/ident)]]}]})
      option-queries)))
