(ns hyperfiddle.foundation.actions
  (:require [cats.core :refer [mlet]]
            [cats.monad.either :as either]
            [hypercrud.browser.routing :as routing]
            [hypercrud.client.peer :as peer]
            [hypercrud.client.schema :as schema]
            [contrib.datomic-tx :as tx]
            [hypercrud.types.DbVal :refer [->DbVal]]
            [hypercrud.util.branch :as branch]
            [contrib.data :refer [map-values map-keys]]
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

(defn hydrate-partition [rt branch on-start dispatch! get-state]
  (dispatch! (apply batch [:hydrate!-start branch] on-start))
  (let [{:keys [stage] :as state} (get-state)
        partition-state (fn [] (get-in state [::runtime/partitions branch]))
        {:keys [route local-basis hydrate-id ::runtime/branch-aux]} (partition-state)]
    (assert route)
    (assert (not (string? route)))
    (-> (runtime/hydrate-route rt local-basis route branch branch-aux stage)
        (p/then (fn [{:keys [ptm tempid-lookups]}]
                  (let [partition-val (partition-state)]
                    (if (= hydrate-id (:hydrate-id partition-val))
                      (let [ptm (map-keys (partial peer/partitioned-request partition-val stage) ptm)]
                        (dispatch! [:hydrate!-success branch ptm tempid-lookups]))
                      (timbre/info (str "Ignoring response for " hydrate-id))))))
        (p/catch (fn [error]
                   (if (= hydrate-id (:hydrate-id (partition-state)))
                     (dispatch! [:partition-error branch error])
                     (timbre/info (str "Ignoring response for " hydrate-id)))
                   (throw error))))))

(defn refresh-global-basis [rt dispatch! get-state]
  (-> (runtime/global-basis rt)
      (p/then (fn [global-basis]
                (dispatch! [:set-global-basis global-basis])))
      (p/catch (fn [error]
                 (dispatch! [:set-error error])
                 (throw error)))))

(defn refresh-partition-basis [rt branch dispatch! get-state]
  (let [{:keys [::runtime/global-basis]} (get-state)
        {:keys [route ::runtime/branch-aux]} (get-in (get-state) [::runtime/partitions branch])]
    (-> (runtime/local-basis rt global-basis route branch branch-aux)
        (p/then (fn [local-basis]
                  (dispatch! [:partition-basis branch local-basis])))
        (p/catch (fn [error]
                   (dispatch! [:partition-error branch error])
                   (throw error))))))

(defn refresh-domain [rt dispatch! get-state]
  (-> (runtime/domain rt)
      (p/then (fn [domain]
                (dispatch! [:hyperfiddle.runtime/set-domain domain])))
      (p/catch (fn [error]
                 (dispatch! [:set-error error])
                 (throw error)))))

(defn add-partition [rt route branch branch-aux & on-start]
  (fn [dispatch! get-state]
    (if-let [e (routing/invalid-route? route)]
      (do
        (dispatch! [:partition-error e])
        (p/rejected e))
      (do
        (dispatch! (apply batch (conj (vec on-start) [:add-partition branch route branch-aux])))
        (-> (refresh-partition-basis rt branch dispatch! get-state)
            (p/then #(hydrate-partition rt branch nil dispatch! get-state)))))))

(defn discard-partition [branch]
  [:discard-partition branch])

(defn close-popover [branch popover-id]
  [:close-popover branch popover-id])

(defn set-route [rt route branch keep-popovers? dispatch! get-state]
  (assert (nil? branch) "Non-nil branches currently unsupported")
  (if-let [e (routing/invalid-route? route)]
    (do (dispatch! [:set-error e])
        (p/rejected e))
    ; currently branches only have relationships to parents, need to be able to find all children from a parent
    ; this would allow us to discard/close ourself and our children
    ; for now we are always nil branch, so blast everything
    (let [actions (->> (::runtime/partitions (get-state))
                       (mapcat (fn [[ident partition]]
                                 (conj (if (and (= branch ident) keep-popovers?)
                                         (vector)
                                         (mapv (partial close-popover ident) (:popovers partition)))
                                       (if (= branch ident)
                                         [:partition-route ident route] ; dont blast nil stage
                                         (discard-partition ident))))))]
      (dispatch! (apply batch actions))
      ; should just call foundation/bootstrap-data
      (-> (refresh-partition-basis rt branch dispatch! get-state)
          (p/then #(hydrate-partition rt branch nil dispatch! get-state))))))

(defn update-to-tempids [get-state branch uri tx]
  (let [{:keys [stage ::runtime/partitions]} (get-state)
        {:keys [tempid-lookups] :as partition-val} (get partitions branch)
        dbval (->DbVal uri branch)
        schema (let [schema-request (schema/schema-request dbval)]
                 (-> (peer/hydrate-val partition-val stage schema-request)
                     (either/branch (fn [e] (throw e)) identity)))
        id->tempid (get tempid-lookups uri)]
    (map (partial tx/stmt-id->tempid id->tempid schema) tx)))

(defn transact! [rt invert-route tx-groups dispatch! get-state & {:keys [route post-tx]}]
  (dispatch! [:transact!-start])
  (let [tx-groups (map-values (partial filter v-not-nil?) ; hack because the ui still generates some garbage tx
                                   tx-groups)]
    (-> (runtime/transact! rt tx-groups)
        (p/catch (fn [error]
                   #?(:cljs (js/alert (pr-str error)))
                   (dispatch! [:transact!-failure error])
                   (throw error)))
        (p/then (fn [{:keys [tempid->id]}]
                  (dispatch! (apply batch [:transact!-success] post-tx))
                  ; todo should just call foundation/bootstrap-data
                  (mlet [_ (refresh-global-basis rt dispatch! get-state)
                         _ (refresh-domain rt dispatch! get-state)
                         :let [invert-id (fn [temp-id uri]
                                           (get-in tempid->id [uri temp-id] temp-id))
                               current-route (get-in (get-state) [::runtime/partitions nil :route])
                               route' (-> (or route current-route)
                                          (invert-route invert-id))
                               keep-popovers? (or (nil? route) (= route current-route))]]
                    ; todo we want to overwrite our current browser location with this new url
                    ; currently this new route breaks the back button
                    (set-route rt route' nil keep-popovers? dispatch! get-state)))))))

(defn should-transact!? [branch get-state]
  (and (nil? branch) (::runtime/auto-transact (get-state))))

(defn with [rt invert-route branch uri tx]
  (fn [dispatch! get-state]
    (let [tx (update-to-tempids get-state branch uri tx)]
      (if (should-transact!? branch get-state)
        (transact! rt invert-route {uri tx} dispatch! get-state)
        (hydrate-partition rt branch [[:with branch uri tx]] dispatch! get-state)))))

(defn open-popover [branch popover-id]
  [:open-popover branch popover-id])

(defn stage-popover [rt invert-route branch swap-fn-async & on-start]
  (fn [dispatch! get-state]
    (p/then (swap-fn-async (get-in (get-state) [:stage branch] {}))
            (fn [{:keys [tx app-route]}]
              (let [with-actions (mapv (fn [[uri tx]]
                                         (let [tx (update-to-tempids get-state branch uri tx)]
                                           [:with branch uri tx]))
                                       tx)
                    parent-branch (branch/decode-parent-branch branch)]
                (if (should-transact!? parent-branch get-state)
                  (do
                    ; should the tx fn not be withd? if the transact! fails, do we want to run it again?
                    (dispatch! (apply batch (concat with-actions on-start)))
                    ; todo what if transact throws?
                    (transact! rt invert-route (get-in (get-state) [:stage branch]) dispatch! get-state
                               {:post-tx [(discard-partition branch)]
                                :route app-route}))
                  (let [e (some-> app-route routing/invalid-route?)
                        actions (concat
                                  with-actions
                                  [[:merge branch]
                                   (cond
                                     e [:set-error e]
                                     app-route [:partition-route nil app-route] ; what about local-basis? why not specify branch?
                                     :else nil)
                                   (discard-partition branch)]
                                  on-start)]
                    (if e
                      (dispatch! (apply batch actions))
                      (hydrate-partition rt parent-branch actions dispatch! get-state)))))))))

(defn reset-stage [rt tx]
  (fn [dispatch! get-state]
    ; check if auto-tx is OFF first?
    (when (not= tx (:stage (get-state)))
      (hydrate-partition rt nil [[:reset-stage tx]] dispatch! get-state))))

(defn manual-transact! [peer invert-route nil-branch-aux]
  (fn [dispatch! get-state]
    ; todo do something when child branches exist and are not nil: hyperfiddle/hyperfiddle#99
    ; can only transact one branch
    (let [tx-groups (get-in (get-state) [:stage nil])]
      (transact! peer invert-route tx-groups dispatch! get-state))))
