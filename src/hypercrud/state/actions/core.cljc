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

(defn hydrate-route* [rt foo branch post-start->route-vals
                      on-start success-action failure-action
                      dispatch! get-state]
  (let [hydrate-id #?(:clj (Math/random) :cljs (js/Math.random))]
    (dispatch! (apply batch [:hydrate!-start hydrate-id] on-start))
    (let [{:keys [stage] :as post-start-state} (get-state)
          {:keys [encoded-route local-basis]} (post-start->route-vals post-start-state)]
      (-> (api/hydrate-route rt local-basis encoded-route foo branch stage)
          (p/then (fn [{:keys [ptm id->tempid]}]
                    (if (= hydrate-id (:hydrate-id (get-state)))
                      (let [stage-val (:stage (get-state))
                            ptm (util/map-keys (fn [request]
                                                 [(branch/branch-vals-for-request request stage-val) request])
                                               ptm)]
                        (dispatch! [success-action ptm id->tempid]))
                      (timbre/info (str "Ignoring response for " hydrate-id)))))
          (p/catch (fn [error]
                     (if (= hydrate-id (:hydrate-id (get-state)))
                       (dispatch! [failure-action error])
                       (timbre/info (str "Ignoring response for " hydrate-id)))
                     (throw error)))))))

(defn hydrate-page [rt on-start dispatch! get-state]
  (hydrate-route* rt {:type "page"} nil #(select-keys % [:encoded-route :local-basis])
                  on-start :hydrate!-success :hydrate!-failure
                  dispatch! get-state))

(defn hydrate-branch [rt post-start->route-vals foo branch
                      on-start dispatch! get-state]
  (hydrate-route* rt foo branch post-start->route-vals
                  on-start :popover-hydrate!-success :popover-hydrate!-failure
                  dispatch! get-state))

(defn refresh-global-basis [rt dispatch! get-state]
  (-> (api/global-basis rt)
      (p/then (fn [global-basis]
                (dispatch! [:set-global-basis global-basis])))
      (p/catch (fn [error]
                 (dispatch! [:set-error error])
                 (throw error)))))

(defn refresh-page-local-basis [rt dispatch! get-state]
  (let [{:keys [global-basis encoded-route]} (get-state)]
    (-> (api/local-basis rt global-basis encoded-route {:type "page"} nil)
        (p/then (fn [local-basis]
                  (dispatch! [:set-local-basis local-basis])))
        (p/catch (fn [error]
                   (dispatch! [:set-error error])
                   (throw error))))))

(defn discard-branch [branch]
  [:discard-branch branch])

(defn close-popover [popover-id]
  [:close-popover popover-id])

(defn set-route [rt encoded-route dispatch! get-state]
  (let [{:keys [branches popovers]} (get-state)
        actions (->> (concat (map discard-branch (keys branches))
                             (map close-popover popovers))
                     (cons [:set-route encoded-route]))]
    (dispatch! (cons :batch actions))
    (-> (refresh-page-local-basis rt dispatch! get-state)
        (p/then (fn [] (hydrate-page rt nil dispatch! get-state))))))

(defn update-to-tempids [get-state branch uri tx]
  (let [{:keys [ptm stage tempid-lookups]} (get-state)
        dbval (->DbVal uri branch)
        schema (let [schema-request (schema/schema-request dbval)]
                 (-> (get ptm schema-request)
                     (api-util/process-result schema-request)
                     (either/branch (fn [e] (throw e)) identity)))
        id->tempid (get-in tempid-lookups [uri branch])]
    (map (partial tx/stmt-id->tempid id->tempid schema) tx)))

(defn with [rt foo branch uri tx]
  (fn [dispatch! get-state]
    (let [tx (update-to-tempids get-state branch uri tx)
          on-start [[:with branch uri tx]]]
      (if (nil? branch)
        (hydrate-page rt on-start dispatch! get-state)
        (let [post-start->route-vals #(get-in % [:branches branch])]
          (hydrate-branch rt post-start->route-vals foo branch on-start dispatch! get-state))))))

(defn open-popover [popover-id]
  [:open-popover popover-id])

(defn open-branched-popover [rt popover-id branch route foo]
  (fn [dispatch! get-state]
    (let [encoded-route (routing/encode route)
          {:keys [global-basis]} (get-state)]
      (-> (api/local-basis rt global-basis encoded-route foo branch)
          (p/catch (fn [error]
                     (dispatch! [:set-error error])
                     (throw error)))
          (p/then (fn [local-basis]
                    (let [on-start [(open-popover popover-id)
                                    [:add-branch branch encoded-route local-basis]]
                          post-start->route-vals (constantly {:encoded-route encoded-route :local-basis local-basis})]
                      (hydrate-branch rt post-start->route-vals foo branch on-start dispatch! get-state))))))))

(defn discard-branched-popover [popover-id branch]
  (batch (discard-branch branch)
         (close-popover popover-id)))

(defn stage-popover [rt popover-id branch foo swap-fn-async]
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
                                 (close-popover popover-id)])]
                  (if-let [parent-branch (branch/decode-parent-branch branch)]
                    (let [post-start->route-vals #(get-in % [:branches parent-branch])]
                      (hydrate-branch rt post-start->route-vals foo parent-branch actions dispatch! get-state))
                    (hydrate-page rt actions dispatch! get-state))))))))

(defn reset-stage [rt tx]
  (fn [dispatch! get-state]
    (when (not= tx (:stage (get-state)))
      (hydrate-page rt [[:reset-stage tx]] dispatch! get-state))))

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
                     #?(:cljs (js/alert (pr-str error)))
                     (dispatch! [:transact!-failure error])
                     (throw error)))
          (p/then (fn [{:keys [tempid->id]}]
                    (let [{:keys [encoded-route]} (get-state)
                          invert-id (fn [temp-id uri]
                                      (get-in tempid->id [uri temp-id] temp-id))
                          updated-route (-> (or (routing/decode encoded-route) home-route)
                                            (routing/invert-ids invert-id ctx)
                                            (routing/encode))]
                      ; todo fix the ordering of this, transact-success should fire immediately (whether or not the rehydrate succeeds)
                      (mlet [_ (refresh-global-basis (:peer ctx) dispatch! get-state)
                             _ (refresh-page-local-basis (:peer ctx) dispatch! get-state)]
                        (hydrate-page (:peer ctx) [[:transact!-success updated-route]] dispatch! get-state)))))))))
