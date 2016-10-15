(ns hypercrud.browser.pages.transact
  (:require [cljs.pprint :as pprint]
            [cljs.reader :as reader]
            [hypercrud.client.core :as hc]
            [hypercrud.ui.code-editor :as code-editor]
            [promesa.core :as p]))


(defn ui [cur transact! graph]
  (let [local-statements (cur [:transact-str])
        last-tx (cur [:last-tx])
        valid? #(try (vector? (reader/read-string %1))
                     (catch :default e false))
        change! (fn [_ [new]]
                  (if (valid? new)
                    (reset! local-statements new)))
        valid?' (valid? @local-statements)]
    [:div
     [:h2 "Transact Datoms"]
     ^{:key (hc/t graph)}
     [code-editor/code-editor* @local-statements change!]
     (if (not valid?') "Invalid tx ")
     [:button {:on-click (fn [] (let [tx (reader/read-string @local-statements)]
                                  (-> (transact! tx)
                                      (p/then (fn [tx-info]
                                                (swap! cur #(-> %
                                                                (assoc :last-tx {:tx-info tx-info
                                                                                 :tx tx})
                                                                (dissoc :transact-str))))))))
               :disabled (not valid?')}
      "Submit"]
     (if-let [{{:keys [tx tempids]} :tx-info stmts :tx} @last-tx]
       [:div
        [:hr]
        [:h2 "Last tx: " tx]
        [:h4 "Datoms:"]
        [:pre (with-out-str (pprint/pprint stmts))]
        [:h4 "Temporary Ids Map:"]
        [:pre (with-out-str (pprint/pprint tempids))]])]))


(defn query []
  ; dummy query so we pull down a graph t value
  nil
  #_{::get-tx ['[:find [?e ...] :in $ ?e :where [?e]] [1] [:db/id]]}) ;why is explicit nil different than nothing
