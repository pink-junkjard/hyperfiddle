(ns hypercrud.state.actions.core
  (:require [hypercrud.state.actions.internal :refer [hydrating-action]]
            [hypercrud.browser.routing :as routing]
            [hypercrud.client.http :as http]
            [promesa.core :as p]))


(defn soft-set-route [route]
  (partial hydrating-action {:on-start (constantly [[:soft-set-route route]])}))

(defn set-route [route]
  (fn [dispatch! get-state]
    (let [state (get-state)]
      (if (not= route (:route state))
        (if (= (some-> state :route :project) (:project route))
          ((soft-set-route route) dispatch! get-state)
          (hydrating-action {:on-start (constantly [[:hard-set-route route]])} dispatch! get-state))))))

(defn set-route-encoded [encoded-route-str]
  (try
    ; bad urls can throw on decode
    (set-route (routing/decode encoded-route-str))
    (catch :default e
      ; todo clean up other state values: remove the response, old route, etc
      [:set-error e])))

(defn discard [conn-id branch]
  (partial hydrating-action {:on-start (constantly [[:discard conn-id branch]])}))

(defn with [conn-id branch tx]
  (partial hydrating-action {:on-start (constantly [[:with conn-id branch tx]])}))

(defn stage-popover [conn-id branch swap-fn]
  (fn [dispatch! get-state]
    (let [tx-from-modal (get-in (get-state) [:stage conn-id branch] [])]
      (p/then (swap-fn tx-from-modal)
              (fn [{:keys [tx app-route]}]
                (let [actions [[:reset-branch conn-id branch tx]
                               [:merge conn-id branch]
                               (if app-route [:soft-set-route app-route])]]
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
