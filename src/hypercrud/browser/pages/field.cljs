(ns hypercrud.browser.pages.field
  (:require [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx-util]
            [hypercrud.form.q-util :as q-util]
            [hypercrud.form.util :as form-util]
            [hypercrud.types :refer [->Entity]]
            [hypercrud.ui.auto-control :refer [auto-control]]))


(defn ui [cur transact! graph entity field navigate-cmp]
  (let [local-statements (cur [:statements] [])
        graph (hc/with graph (-> entity .-dbval .-dbval) @local-statements)
        entity (->Entity (.-dbid entity) (hc/get-dbgraph graph (-> entity .-dbval .-dbval)))
        stage-tx! #(swap! local-statements tx-util/into-tx %)
        expanded-cur (cur [:expanded (-> field :field/attribute :attribute/ident)]
                          ; hacky but we currently only want expanded edit forms where we draw tables
                          (if (= :db.cardinality/many (:cardinality field)) {} nil))]
    [:div
     [auto-control entity {:expanded-cur expanded-cur
                           :field field
                           :graph graph
                           :navigate-cmp navigate-cmp
                           :stage-tx! stage-tx!}]
     [:button {:on-click #(transact! @local-statements)
               :disabled (empty? @local-statements)}
      "Update"]]))


(defn query [state dbid field param-ctx]
  (let [param-ctx (merge param-ctx {:id (.-id dbid)})
        dbval (get param-ctx :dbval)
        expanded-forms (get state :expanded nil)
        p-filler q-util/build-params-from-formula
        option-queries (form-util/field-queries expanded-forms p-filler param-ctx field form-util/recurse?)]
    (if (not (tx-util/tempid? dbid))
      (merge option-queries
             (let [q '[:find [?e ...] :in $ ?e :where [?e]]
                   pull-exp [:db/id (form-util/field-pull-exp-entry expanded-forms field form-util/expand?)]]
               {dbid [q [dbval (.-id dbid)] [dbval pull-exp]]}))
      option-queries)))
