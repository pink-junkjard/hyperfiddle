(ns hypercrud.state.actions.core
  (:require [cats.monad.either :as either]
            [hypercrud.browser.routing :as routing]
            [hypercrud.client.http :as http]
            [hypercrud.client.peer :as peer]
            [hypercrud.client.schema :as schema]
            [hypercrud.client.tx :as tx]
            [hypercrud.state.actions.internal :refer [hydrating-action]]
            [hypercrud.state.core :as state]
            [hypercrud.types.DbVal :refer [->DbVal]]
            [hypercrud.util.branch :as branch]
            [promesa.core :as p]))


(defn set-route [route]
  (fn [dispatch! get-state]
    (let [state (get-state)]
      (if (not= route (:encoded-route state))
        (hydrating-action {:on-start (constantly [[:set-route route]])} dispatch! get-state)))))

(defn update-to-tempids [get-state branch uri tx]
  (let [{:keys [ptm stage tempid-lookups]} (get-state)
        branch-val (branch/branch-val uri branch stage)
        dbval (->DbVal uri branch-val)
        schema (let [schema-request (schema/schema-request dbval)]
                 (-> (get ptm schema-request)
                     (peer/process-result schema-request)
                     (either/branch (fn [e] (throw e)) identity)))
        id->tempid (get-in tempid-lookups [uri branch-val])]
    (map (partial tx/stmt-id->tempid id->tempid schema) tx)))

(defn with [branch uri tx]
  (fn [dispatch! get-state]
    (let [tx (update-to-tempids get-state branch uri tx)]
      (hydrating-action {:on-start (constantly [[:with branch uri tx]])} dispatch! get-state))))

(defn open-popover [popover-id]
  (partial hydrating-action {:on-start (constantly [[:open-popover popover-id]])}))

(defn cancel-popover [branch popover-id]
  (partial hydrating-action {:on-start (constantly [[:discard branch]
                                                    [:close-popover popover-id]])}))

(defn stage-popover [branch popover-id swap-fn-async]
  (fn [dispatch! get-state]
    (let [multi-color-tx (get-in (get-state) [:stage branch] {})]
      (p/then (swap-fn-async multi-color-tx)
              (fn [{:keys [tx app-route]}]
                (let [actions (concat
                                (mapv (fn [[uri tx]]
                                        (let [tx (update-to-tempids get-state branch uri tx)]
                                          [:with branch uri tx]))
                                      tx)
                                [[:merge branch]
                                 (if app-route [:set-route (routing/encode app-route)])
                                 [:close-popover popover-id]])]
                  (hydrating-action {:on-start (constantly actions)} dispatch! get-state)))))))

(defn reset-stage [tx]
  (fn [dispatch! get-state]
    (when (not= tx (:stage (get-state)))
      (hydrating-action {:on-start (constantly [[:reset-stage tx]])} dispatch! get-state))))

(defn transact! [home-route ctx]
  (fn [dispatch! get-state]
    (dispatch! [:transact!-start])
    (let [{:keys [stage]} (get-state)]
      (-> (http/transact! state/*service-uri* stage)
          (p/catch (fn [error]
                     (js/alert (pr-str error))
                     (dispatch! [:transact!-failure error])
                     (p/rejected error)))
          (p/then (fn [{:keys [tempid->id]}]
                    (let [{:keys [encoded-route]} (get-state)
                          invert-id (fn [temp-id uri]
                                      (get-in tempid->id [uri temp-id] temp-id))
                          updated-route (-> (or (routing/decode encoded-route) home-route)
                                            (routing/invert-ids invert-id ctx)
                                            (routing/encode))]
                      (hydrating-action {:on-start (constantly [[:transact!-success updated-route]])} dispatch! get-state))))))))
