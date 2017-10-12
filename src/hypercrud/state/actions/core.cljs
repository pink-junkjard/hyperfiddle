(ns hypercrud.state.actions.core
  (:require [clojure.set :as set]
            [hypercrud.browser.routing :as routing]
            [hypercrud.client.http :as http]
            [hypercrud.client.temp :as temp]
            [hypercrud.state.actions.internal :refer [hydrating-action]]
            [hypercrud.util.core :as util]
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

(defn transact! [home-route]
  (fn [dispatch! get-state]
    (dispatch! [:transact!-start])
    (let [{:keys [entry-uri stage]} (get-state)]
      (-> (http/transact! entry-uri stage)
          (p/catch (fn [error]
                     (js/alert (pr-str error))
                     (dispatch! [:transact!-failure error])
                     (p/rejected error)))
          (p/then (fn [{:keys [tempids]}]
                    (let [{:keys [encoded-route]} (get-state)
                          ; todo this id-lookup-by-conn should be the data structure the server returns
                          id-lookup-by-conn (->> tempids
                                                 (group-by (fn [[temp-dbid dbid]] (:uri temp-dbid)))
                                                 (util/map-values (fn [kvs]
                                                                    (->> kvs
                                                                         (map (partial mapv :id))
                                                                         (into {})
                                                                         (set/map-invert)))))
                          updated-route (-> (or (routing/decode encoded-route) home-route)
                                            (routing/invert-dbids (fn [dbid]
                                                                    (let [id-lookup (get id-lookup-by-conn (:uri dbid))]
                                                                      (temp/tempdbid->dbid id-lookup dbid))))
                                            (routing/encode))]
                      (hydrating-action {:on-start (constantly [[:transact!-success updated-route]])} dispatch! get-state))))))))
