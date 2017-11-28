(ns hypercrud.state.actions.core
  (:require [cats.monad.either :as either]
            [hypercrud.browser.routing :as routing]
            [hypercrud.client.transact :as origin]
            [hypercrud.client.process-result :as process-result]
            [hypercrud.client.schema :as schema]
            [hypercrud.client.tx :as tx]
            [hypercrud.hydrating-action :refer [hydrating-action]] ; platform injected
            [hypercrud.state.core :as state]
            [hypercrud.types.DbVal :refer [->DbVal]]
            [hypercrud.util.branch :as branch]
            [promesa.core :as p]))


(defn set-route [encoded-route dispatch! get-state]
  (let [actions (->> (:popovers (get-state))
                     (mapcat (fn [branch]
                               [[:discard branch]
                                [:close-popover branch]]))
                     (cons [:set-route encoded-route]))]
    (hydrating-action {:on-start (constantly actions)} dispatch! get-state)))

(defn update-to-tempids [get-state branch uri tx]
  (let [{:keys [ptm stage tempid-lookups]} (get-state)
        branch-val (branch/branch-val uri branch stage)
        dbval (->DbVal uri branch-val)
        schema (let [schema-request (schema/schema-request dbval)]
                 (-> (get ptm schema-request)
                     (process-result/process-result schema-request)
                     (either/branch (fn [e] (throw e)) identity)))
        id->tempid (get-in tempid-lookups [uri branch-val])]
    (map (partial tx/stmt-id->tempid id->tempid schema) tx)))

(defn with [branch uri tx]
  (fn [dispatch! get-state]
    (let [tx (update-to-tempids get-state branch uri tx)]
      (hydrating-action {:on-start (constantly [[:with branch uri tx]])} dispatch! get-state))))

(defn open-popover [branch]
  (partial hydrating-action {:on-start (constantly [[:open-popover branch]])}))

(defn cancel-popover [branch]
  (partial hydrating-action {:on-start (constantly [[:discard branch]
                                                    [:close-popover branch]])}))

(defn stage-popover [branch swap-fn-async]
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
                                 [:close-popover branch]])]
                  (hydrating-action {:on-start (constantly actions)} dispatch! get-state)))))))

(defn reset-stage [tx]
  (fn [dispatch! get-state]
    (when (not= tx (:stage (get-state)))
      (hydrating-action {:on-start (constantly [[:reset-stage tx]])} dispatch! get-state))))

(defn transact! [home-route ctx]
  (fn [dispatch! get-state]
    (dispatch! [:transact!-start])
    (let [{:keys [stage]} (get-state)]
      (-> (origin/transact! state/*service-uri* stage)
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
