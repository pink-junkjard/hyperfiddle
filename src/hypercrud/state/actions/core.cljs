(ns hypercrud.state.actions.core
  (:require [hypercrud.state.actions.internal :refer [hydrating-action]]
            [hypercrud.browser.routing :as routing]
            [hypercrud.client.http :as http]
            [promesa.core :as p]))


(defn set-route [route]
  (fn [dispatch! get-state]
    (let [state (get-state)]
      (if (not= route (:encoded-route state))
        (hydrating-action {:on-start (constantly [[:set-route route]])} dispatch! get-state)))))

(defn with [branch uri tx]
  (partial hydrating-action {:on-start (constantly [[:with branch uri tx]])}))

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
                                (mapv (fn [[uri tx]] [:with branch uri tx]) tx)
                                [[:merge branch]
                                 (if app-route [:set-route (routing/encode app-route)])
                                 [:close-popover popover-id]])]
                  (hydrating-action {:on-start (constantly actions)} dispatch! get-state)))))))

(defn reset-stage [tx]
  (fn [dispatch! get-state]
    (when (not= tx (:stage (get-state)))
      (hydrating-action {:on-start (constantly [[:reset-stage tx]])} dispatch! get-state))))

(defn transact! []
  (fn [dispatch! get-state]
    (dispatch! [:transact!-start])
    (let [{:keys [entry-uri stage]} (get-state)]
      (-> (http/transact! entry-uri stage)
          (p/catch (fn [error]
                     (js/alert (pr-str error))
                     (dispatch! [:transact!-failure error])
                     (p/rejected error)))
          (p/then (fn [{:keys [tempids]}]
                    (hydrating-action {:on-start (constantly [[:transact!-success tempids]])} dispatch! get-state)))))))
