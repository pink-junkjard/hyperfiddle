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

(defn hydrate-partition [rt branch on-start dispatch! get-state]
  (dispatch! (apply batch [:hydrate!-start branch] on-start))
  (let [{:keys [stage] :as state} (get-state)
        branch-state (fn [] (get-in state [::runtime/partitions branch]))
        {:keys [route local-basis hydrate-id ::runtime/branch-aux]} (branch-state)]
    (assert route)
    (assert (not (string? route)))
    (-> (runtime/hydrate-route rt local-basis route branch branch-aux stage)
        (p/then (fn [{:keys [ptm tempid-lookups]}]
                  (if (= hydrate-id (:hydrate-id (branch-state)))
                    (let [ptm (util/map-keys (fn [request]
                                               [(branch/branch-vals-for-request request stage) request])
                                             ptm)]
                      (dispatch! [:hydrate!-success branch ptm tempid-lookups]))
                    (timbre/info (str "Ignoring response for " hydrate-id)))))
        (p/catch (fn [error]
                   (if (= hydrate-id (:hydrate-id (branch-state)))
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
    (dispatch! (apply batch (conj (vec on-start) [:add-partition branch route branch-aux])))
    (-> (refresh-partition-basis rt branch dispatch! get-state)
        (p/then #(hydrate-partition rt branch nil dispatch! get-state)))))

(defn discard-partition [branch]
  [:discard-partition branch])

(defn close-popover [branch popover-id]
  [:close-popover branch popover-id])

(defn set-route [rt route branch branch-aux dispatch! get-state]
  (assert (nil? branch) "Non-nil branches currently unsupported")
  ; currently branches only have relationships to parents, need to be able to find all children from a parent
  ; this would allow us to discard/close ourself and our children
  ; for now we are always nil branch, so blast everything
  (let [actions (->> (::runtime/partitions (get-state))
                     (mapcat (fn [[ident partition]]
                               (conj (mapv (partial close-popover ident) (:popovers partition))
                                     (if (= branch ident)
                                       [:partition-route ident route] ; dont blast nil stage
                                       (discard-partition ident))))))]
    (dispatch! (apply batch actions))
    ; should just call foundation/bootstrap-data
    (-> (refresh-partition-basis rt branch dispatch! get-state)
        (p/then #(hydrate-partition rt branch nil dispatch! get-state)))))

(defn update-to-tempids [get-state branch uri tx]
  (let [{:keys [stage ::runtime/partitions]} (get-state)
        {:keys [ptm tempid-lookups]} (get partitions branch)
        dbval (->DbVal uri branch)
        schema (let [schema-request (schema/schema-request dbval)]
                 (-> (peer/hydrate-val ptm stage schema-request)
                     (either/branch (fn [e] (throw e)) identity)))
        id->tempid (get tempid-lookups uri)]
    (map (partial tx/stmt-id->tempid id->tempid schema) tx)))

(defn with [rt branch uri tx]
  (fn [dispatch! get-state]
    (let [tx (update-to-tempids get-state branch uri tx)
          on-start [[:with branch uri tx]]]
      (hydrate-partition rt branch on-start dispatch! get-state))))

(defn open-popover [branch popover-id]
  [:open-popover branch popover-id])

(defn stage-popover [rt branch swap-fn-async & on-start]
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
                                 (if app-route [:partition-route nil app-route]) ; what about local-basis? why not specify branch?
                                 (discard-partition branch)]
                                on-start)]
                  (let [parent-branch (branch/decode-parent-branch branch)]
                    (hydrate-partition rt parent-branch actions dispatch! get-state))))))))

(defn reset-stage [rt tx]
  (fn [dispatch! get-state]
    (when (not= tx (:stage (get-state)))
      (hydrate-partition rt nil [[:reset-stage tx]] dispatch! get-state))))

(defn transact! [peer target-repository nil-branch-aux]
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
                    (let [route (get-in (get-state) [::runtime/partitions nil :route])
                          invert-id (fn [temp-id uri]
                                      (get-in tempid->id [uri temp-id] temp-id))
                          route' (routing/invert-ids route invert-id target-repository)]
                      (dispatch! [:transact!-success])
                      ; todo should just call foundation/bootstrap-data
                      (mlet [_ (refresh-global-basis peer dispatch! get-state)
                             _ (refresh-domain peer dispatch! get-state)]
                        ; todo we want to overwrite our current browser location with this new url
                        ; currently this new route breaks the back button
                        (set-route peer route' nil nil-branch-aux dispatch! get-state)))))))))
