(ns hypercrud.browser.pages.query
  (:require [cljs.pprint :as pprint]
            [hypercrud.ui.table :as table]
            [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx-util]))


(defn ui [cur transact! graph forms form-id query-blob navigate-cmp]
  (let [local-statements (cur [:statements] [])
        expanded-cur (cur [:expanded] {})
        graph (hc/with graph @local-statements)
        stage-tx! #(swap! local-statements tx-util/into-tx %)]
    [:div
     [:pre (with-out-str (pprint/pprint query-blob))]
     [table/table graph (hc/select graph ::table/query) forms form-id expanded-cur stage-tx! navigate-cmp nil]
     [:button {:key 1 :on-click #(transact! @local-statements)} "Save"]

     ;todo just add row at bottom
     [navigate-cmp {:href (str form-id "/entity/-1")} "Create"]]))


(defn query [state query-blob forms form-id]
  (table/query query-blob forms form-id (get state :expanded nil)))
