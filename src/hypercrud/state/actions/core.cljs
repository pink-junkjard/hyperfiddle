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

(defn set-route-encoded [encoded-route-str index-route]
  (try
    ; bad urls can throw on decode
    (set-route (or (routing/decode encoded-route-str) index-route))
    (catch :default e
      ; todo clean up other state values: remove the response, old route, etc
      [:set-error e])))

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
              (fn [{:keys [multi-color-tx app-route]}]
                (let [actions (concat
                                (mapv (fn [[uri tx]] [:with branch uri tx]) multi-color-tx)
                                [[:merge branch]
                                 (if app-route [:soft-set-route app-route])
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
