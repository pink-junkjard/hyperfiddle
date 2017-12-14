(ns hypercrud.state.actions.core
  (:require [cats.core :refer [mlet]]
            [cats.monad.either :as either]
            [hypercrud.api.core :as api]
            [hypercrud.api.util :as api-util]
            [hypercrud.browser.routing :as routing]
            [hypercrud.client.schema :as schema]
            [hypercrud.client.tx :as tx]
            [hypercrud.types.DbVal :refer [->DbVal]]
            [hypercrud.util.branch :as branch]
            [hypercrud.util.core :as util]
            [promesa.core :as p]
            [taoensso.timbre :as timbre]))


; batch doesn't make sense with thunks (can be sync or async dispatches in a thunk),
; user beware
(defn batch [& action-list] (cons :batch action-list))

(defn rehydrate [rt on-start dispatch! get-state]
  (let [hydrate-id (js/Math.random)]
    (dispatch! (apply batch [:hydrate!-start hydrate-id] on-start))
    (-> (api/hydrate-route rt)
        (p/then (fn [{:keys [ptm id->tempid]}]
                  (if (= hydrate-id (:hydrate-id (get-state)))
                    (dispatch! [:hydrate!-success ptm id->tempid])
                    (timbre/info (str "Ignoring response for " hydrate-id)))))
        (p/catch (fn [error]
                   (if (= hydrate-id (:hydrate-id (get-state)))
                     (dispatch! [:hydrate!-failure error])
                     (timbre/info (str "Ignoring response for " hydrate-id)))
                   (p/rejected error))))))

(defn refresh-global-basis [rt dispatch! get-state]
  (-> (api/global-basis rt)
      (p/then (fn [global-basis]
                (dispatch! [:set-global-basis global-basis])))
      (p/catch (fn [error]
                 (dispatch! [:set-error error])
                 (p/rejected error)))))

(defn refresh-local-basis [rt dispatch! get-state]
  (-> (api/local-basis rt)
      (p/then (fn [local-basis]
                (dispatch! [:set-local-basis local-basis])))
      (p/catch (fn [error]
                 (dispatch! [:set-error error])
                 (p/rejected error)))))

(defn set-route [rt encoded-route dispatch! get-state]
  (let [actions (cons [:set-route encoded-route]
                      (mapcat (fn [branch]
                                [[:discard branch]
                                 [:close-popover branch]])
                              (:popovers (get-state))))]
    (dispatch! (cons :batch actions))
    (-> (refresh-local-basis rt dispatch! get-state)
        (p/then (fn [] (rehydrate rt nil dispatch! get-state))))))

(defn update-to-tempids [get-state branch uri tx]
  (let [{:keys [ptm stage tempid-lookups]} (get-state)
        branch-val (branch/branch-val uri branch stage)
        dbval (->DbVal uri branch-val)
        schema (let [schema-request (schema/schema-request dbval)]
                 (-> (get ptm schema-request)
                     (api-util/process-result schema-request)
                     (either/branch (fn [e] (throw e)) identity)))
        id->tempid (get-in tempid-lookups [uri branch-val])]
    (map (partial tx/stmt-id->tempid id->tempid schema) tx)))

(defn with [rt branch uri tx]
  (fn [dispatch! get-state]
    (let [tx (update-to-tempids get-state branch uri tx)]
      (rehydrate rt [[:with branch uri tx]] dispatch! get-state))))

(defn open-popover [branch]
  [:open-popover branch])

(defn cancel-popover [rt branch]
  (partial rehydrate rt [[:discard branch]
                         [:close-popover branch]]))

(defn stage-popover [rt branch swap-fn-async]
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
                  (rehydrate rt actions dispatch! get-state)))))))

(defn reset-stage [rt tx]
  (fn [dispatch! get-state]
    (when (not= tx (:stage (get-state)))
      (rehydrate rt [[:reset-stage tx]] dispatch! get-state))))

(defn transact! [home-route ctx]
  (fn [dispatch! get-state]
    (dispatch! [:transact!-start])
    (let [{:keys [stage]} (get-state)
          ; todo do something when child branches exist and are not nil: hyperfiddle/hyperfiddle#99
          tx-groups (->> (get stage nil)                    ; can only transact one branch
                         ; hack because the ui still generates some garbage tx
                         (util/map-values (partial filter api-util/v-not-nil?)))]
      (-> (api/transact! (:peer ctx) tx-groups)
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
                      ; todo fix the ordering of this, transact-success should fire immediately (whether or not the rehydrate succeeds)
                      (mlet [_ (refresh-global-basis (:peer ctx) dispatch! get-state)
                             _ (refresh-local-basis (:peer ctx) dispatch! get-state)]
                        (rehydrate (:peer ctx) [[:transact!-success updated-route]] dispatch! get-state)))))))))
