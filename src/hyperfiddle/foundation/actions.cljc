(ns hyperfiddle.foundation.actions
  (:require [cats.core :refer [mlet]]
            [cats.monad.either :as either]
            [hypercrud.browser.routing :as routing]
            [hypercrud.client.peer :as peer]
            [hypercrud.client.schema :as schema]
            [hypercrud.client.tx :as tx]
            [hypercrud.types.DbVal :refer [->DbVal]]
            [hypercrud.util.branch :as branch]
            [hypercrud.util.core :as util]
            [hyperfiddle.io.util :refer [v-not-nil?]]
            [hyperfiddle.runtime :as runtime]
            [promesa.core :as p]
            [taoensso.timbre :as timbre]))


(defn toggle-staging [] [:toggle-staging])

(defn set-display-mode [display-mode]
  [:set-display-mode display-mode])

; batch doesn't make sense with thunks (can be sync or async dispatches in a thunk),
; user beware
(defn batch [& action-list] (cons :batch action-list))

(defn hydrate-route* [rt page-or-leaf branch post-start->route-vals
                      on-start success-action failure-action
                      dispatch! get-state]
  (let [hydrate-id #?(:clj (Math/random) :cljs (js/Math.random))]
    (dispatch! (apply batch [:hydrate!-start hydrate-id] on-start))
    (let [{:keys [stage] :as post-start-state} (get-state)
          {:keys [route local-basis]} (post-start->route-vals post-start-state)]
      (assert route)
      (assert (not (string? route)))
      (-> (case page-or-leaf
            :page (runtime/hydrate-route-page rt local-basis route stage)
            :leaf (runtime/hydrate-route rt local-basis route branch stage))
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
  (hydrate-route* rt :page nil #(select-keys % [:route :local-basis])
                  on-start :hydrate!-success :hydrate!-failure
                  dispatch! get-state))

(defn hydrate-branch [rt branch on-start dispatch! get-state]
  (hydrate-route* rt :leaf branch #(get-in % [:branches branch])
                  on-start :popover-hydrate!-success :popover-hydrate!-failure
                  dispatch! get-state))

(defn refresh-global-basis [rt dispatch! get-state]
  (-> (runtime/global-basis rt)
      (p/then (fn [global-basis]
                (dispatch! [:set-global-basis global-basis])))
      (p/catch (fn [error]
                 (dispatch! [:set-error error])
                 (throw error)))))

(defn refresh-page-local-basis [rt dispatch! get-state]
  (let [{:keys [global-basis route]} (get-state)]
    (-> (runtime/local-basis-page rt global-basis route)
        (p/then (fn [local-basis]
                  (dispatch! [:set-local-basis local-basis])))
        (p/catch (fn [error]
                   (dispatch! [:set-error error])
                   (throw error))))))

(defn refresh-domain [rt dispatch! get-state]
  (-> (runtime/domain rt)
      (p/then (fn [domain]
                (dispatch! [:hyperfiddle.runtime/set-domain domain])))
      (p/catch (fn [error]
                 (dispatch! [:set-error error])
                 (throw error)))))

(defn discard-branch [branch]
  [:discard-branch branch])

(defn close-popover [popover-id]
  [:close-popover popover-id])

(defn set-route [rt-page route dispatch! get-state]
  (let [{:keys [branches popovers]} (get-state)
        actions (->> (concat (map discard-branch (keys branches))
                             (map close-popover popovers))
                     (cons [:set-route route]))]
    (dispatch! (cons :batch actions))
    (-> (refresh-page-local-basis rt-page dispatch! get-state)
        (p/then (fn [] (hydrate-page rt-page nil dispatch! get-state))))))

(defn update-to-tempids [get-state branch uri tx]
  (let [{:keys [ptm stage tempid-lookups]} (get-state)
        dbval (->DbVal uri branch)
        schema (let [schema-request (schema/schema-request dbval)]
                 (-> (peer/hydrate-val ptm stage schema-request)
                     (either/branch (fn [e] (throw e)) identity)))
        id->tempid (get-in tempid-lookups [uri branch])]
    (map (partial tx/stmt-id->tempid id->tempid schema) tx)))

(defn with [rt branch uri tx]
  (fn [dispatch! get-state]
    (let [tx (update-to-tempids get-state branch uri tx)
          on-start [[:with branch uri tx]]]
      (if (nil? branch)
        (hydrate-page rt on-start dispatch! get-state)
        (hydrate-branch rt branch on-start dispatch! get-state)))))

(defn open-popover [popover-id]
  [:open-popover popover-id])

(defn open-branched-popover [rt popover-id branch route]
  (fn [dispatch! get-state]
    (let [{:keys [global-basis]} (get-state)]
      (-> (runtime/local-basis rt global-basis route branch)
          (p/catch (fn [error]
                     (dispatch! [:set-error error])
                     (throw error)))
          (p/then (fn [local-basis]
                    (let [on-start [(open-popover popover-id)
                                    [:add-branch branch route local-basis]]]
                      (hydrate-branch rt branch on-start dispatch! get-state))))))))

(defn discard-branched-popover [popover-id branch]
  (batch (discard-branch branch)
         (close-popover popover-id)))

(defn stage-popover [rt popover-id branch swap-fn-async]
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
                                 (if app-route [:set-route app-route])
                                 (close-popover popover-id)
                                 (discard-branch branch)])]
                  (if-let [parent-branch (branch/decode-parent-branch branch)]
                    (hydrate-branch rt parent-branch actions dispatch! get-state)
                    (hydrate-page rt actions dispatch! get-state))))))))

(defn reset-stage [rt tx]
  (fn [dispatch! get-state]
    (when (not= tx (:stage (get-state)))
      (hydrate-page rt [[:reset-stage tx]] dispatch! get-state))))

(defn transact! [peer target-repository]
  (fn [dispatch! get-state]
    (dispatch! [:transact!-start])
    (let [{:keys [stage]} (get-state)
          ; todo do something when child branches exist and are not nil: hyperfiddle/hyperfiddle#99
          tx-groups (->> (get stage nil)                    ; can only transact one branch
                         ; hack because the ui still generates some garbage tx
                         (util/map-values (partial filter v-not-nil?)))]
      (-> (runtime/transact! peer tx-groups)
          (p/catch (fn [error]
                     #?(:cljs (js/alert (pr-str error)))
                     (dispatch! [:transact!-failure error])
                     (throw error)))
          (p/then (fn [{:keys [tempid->id]}]
                    (let [{:keys [route]} (get-state)
                          invert-id (fn [temp-id uri]
                                      (get-in tempid->id [uri temp-id] temp-id))
                          route' (routing/invert-ids route invert-id target-repository)]
                      ; todo fix the ordering of this, transact-success should fire immediately (whether or not the rehydrate succeeds)
                      ; todo should just call foundation/bootstrap-data
                      (mlet [_ (refresh-global-basis peer dispatch! get-state)
                             _ (refresh-domain peer dispatch! get-state)
                             _ (refresh-page-local-basis peer dispatch! get-state)]
                        (hydrate-page peer [[:transact!-success route']] dispatch! get-state)))))))))
