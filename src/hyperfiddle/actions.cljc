(ns hyperfiddle.actions
  (:require
    [cats.core :refer [mlet]]
    [cats.labs.promise]
    [cats.monad.either :as either]
    [clojure.spec.alpha :as s]
    [contrib.data :as data]
    [contrib.datomic-tx :as tx]
    [hypercrud.types.Err :as Err]
    [hypercrud.util.branch :as branch]
    [hyperfiddle.io.core :as io]
    [hyperfiddle.route :as route]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.schema :as schema]
    [promesa.core :as p]
    [taoensso.timbre :as timbre]))


; batch doesn't make sense with thunks (can be sync or async dispatches in a thunk),
; user beware
(defn batch [& action-list] (cons :batch action-list))

(defn hydrate-partition [rt branch dispatch! get-state]
  (let [{:keys [hydrate-id local-basis route]} (get-in (get-state) [::runtime/partitions branch])
        stage (data/map-values :stage @(runtime/state rt [::runtime/partitions]))]
    (-> (io/hydrate-route (runtime/io rt) local-basis route branch stage)
        (p/then (fn [{:keys [local-basis ptm tempid-lookups]}]
                  (if (= hydrate-id (get-in (get-state) [::runtime/partitions branch :hydrate-id]))
                    (dispatch! [:hydrate!-route-success branch ptm tempid-lookups local-basis]) ; todo domain & user are now potentially out of sync
                    (timbre/info (str "Ignoring response for " hydrate-id)))))
        (p/catch (fn [error]
                   (if (= hydrate-id (get-in (get-state) [::runtime/partitions branch :hydrate-id]))
                     (dispatch! [:partition-error branch error])
                     (timbre/info (str "Ignoring response for " hydrate-id)))
                   (throw error))))))

(defn refresh-global-basis [rt on-finally dispatch! get-state] ; on-finally is a crude hack
  (-> (io/global-basis (runtime/io rt))
      (p/then (fn [global-basis]
                (dispatch! (apply batch [:set-global-basis global-basis] on-finally))))
      (p/catch (fn [error]
                 (dispatch! (apply batch [:set-error error] on-finally))
                 (throw error)))))

(defn refresh-partition-basis [rt branch dispatch! get-state]
  (let [global-basis @(runtime/state rt [::runtime/global-basis])
        route @(runtime/state rt [::runtime/partitions branch :route])]
    (-> (io/local-basis (runtime/io rt) global-basis route)
        (p/then (fn [local-basis]
                  (dispatch! [:partition-basis branch local-basis])))
        (p/catch (fn [error]
                   (dispatch! [:partition-error branch error])
                   (throw error))))))

(defn hydrate-partition-schema [rt branch dispatch! get-state]
  (-> (schema/hydrate-schemas rt branch)
      (p/then (fn [schema]
                (dispatch! [:partition-schema branch schema])))
      (p/catch (fn [error]
                 (dispatch! [:partition-error branch error])
                 (throw error)))))

(defn add-partition [rt route branch & on-start]
  {:pre [(s/valid? :hyperfiddle/route route)]}
  (fn [dispatch! get-state]
    (dispatch! (apply batch (conj (vec on-start) [:add-partition branch route])))
    (-> (refresh-partition-basis rt branch dispatch! get-state)
        (p/then #(dispatch! [:hydrate!-start branch]))
        (p/then #(hydrate-partition-schema rt branch dispatch! get-state))
        (p/then #(hydrate-partition rt branch dispatch! get-state)))))

(defn discard-partition [branch]
  [:discard-partition branch])

(defn close-popover [branch popover-id]
  [:close-popover branch popover-id])

(defn set-route [rt route branch keep-popovers? force dispatch! get-state]
  {:pre [(s/valid? :hyperfiddle/route route)]}
  (assert (nil? branch) "Non-nil branches currently unsupported")
  (let [current-route (get-in (get-state) [::runtime/partitions branch :route])]
    (if (and (not force) (route/compare-routes route current-route) (not= route current-route))
      (dispatch! [:partition-route branch route])           ; just update state without re-hydrating
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
            (p/then #(dispatch! [:hydrate!-start branch]))
            (p/then #(hydrate-partition-schema rt branch dispatch! get-state))
            (p/then #(hydrate-partition rt branch dispatch! get-state)))))))

(defn update-to-tempids [get-state branch dbname tx]
  (let [{:keys [tempid-lookups schemas]} (get-in (get-state) [::runtime/partitions branch])
        schema (get schemas dbname)
        id->tempid (some-> (get tempid-lookups dbname)
                           (either/branch #(throw (ex-info % {})) identity))]
    (map (partial tx/stmt-id->tempid id->tempid schema) tx)))

(defn transact! [rt tx-groups dispatch! get-state & {:keys [route post-tx]}]
  (dispatch! [:transact!-start])
  (-> (io/transact! (runtime/io rt) tx-groups)
      (p/catch (fn [e]
                 #?(:cljs
                    (let [message (cond
                                    (string? e) e
                                    (Err/Err? e) (:msg e)
                                    (map? e) (:message e)
                                    :else (ex-message e))]
                      (if (= "Please refresh your browser" message)
                        (js/alert "We deployed some updates and your client is out of date, press OK and we will refresh your page")
                        (js/alert message))))
                 (dispatch! [:transact!-failure e])
                 (throw e)))
      (p/then (fn [{:keys [tempid->id]}]
                ; todo should just call foundation/bootstrap-data
                (mlet [:let [on-finally (into [[:transact!-success (keys tx-groups)]] post-tx)]
                       _ (refresh-global-basis rt on-finally dispatch! get-state)
                       :let [current-route (get-in (get-state) [::runtime/partitions nil :route])]]
                  (either/branch
                    (or (some-> route route/validate-route+) ; arbitrary user input, need to validate
                        (either/right current-route))
                    (fn [e]
                      (dispatch! [:set-error e])
                      (p/rejected e))
                    (fn [route]
                      (let [invert-id (fn [dbname id] (get-in tempid->id [dbname id] id))
                            route' (route/invert-route route invert-id)
                            keep-popovers? (or (nil? route) (route/compare-routes route current-route))]
                        ; todo we want to overwrite our current browser location with this new url
                        ; currently this new route breaks the back button
                        (set-route rt route' nil keep-popovers? true dispatch! get-state)))))))))

(defn should-transact!? [dbname get-state]
  (get-in (get-state) [::runtime/auto-transact dbname]))

(defn with-groups [rt branch tx-groups & {:keys [route post-tx]}]
  {:pre [(not-any? nil? (keys tx-groups))]}
  (fn [dispatch! get-state]
    (let [tx-groups (->> tx-groups
                         (remove (fn [[dbname tx]] (empty? tx)))
                         (map (fn [[dbname tx]] [dbname (update-to-tempids get-state branch dbname tx)]))
                         (into {}))
          transact-dbnames (->> (keys tx-groups)
                                (filter (fn [dbname] (and (nil? branch) (should-transact!? dbname get-state)))))
          transact-groups (select-keys tx-groups transact-dbnames)
          with-actions (->> (apply dissoc tx-groups transact-dbnames)
                            (mapv (fn [[dbname tx]] [:with branch dbname tx])))]
      (if (not (empty? transact-groups))
        ; todo what if transact throws?
        (transact! rt transact-groups dispatch! get-state
                   :post-tx (let [clear-uris (->> (keys transact-groups)
                                                  (map (fn [dbname] [:reset-stage-db branch dbname nil]))
                                                  vec)]
                              (concat clear-uris            ; clear the uris that were transacted
                                      with-actions))
                   :route route)
        (either/branch
          (or (some-> route route/validate-route+) (either/right nil)) ; arbitrary user input, need to validate
          (fn [e]
            (dispatch! (apply batch (conj with-actions [:set-error e]))))
          (fn [route]
            (let [actions (cond-> with-actions
                            ; what about local-basis? why not specify branch?
                            route (conj [:partition-route nil route]))]
              (dispatch! (apply batch (conj actions [:hydrate!-start branch])))
              (hydrate-partition rt branch dispatch! get-state))))))))

(defn open-popover [branch popover-id]
  [:open-popover branch popover-id])

(defn stage-popover [rt branch swap-fn-async & on-start]    ; todo rewrite in terms of with-groups
  (fn [dispatch! get-state]
    (p/then (swap-fn-async)
            (fn [{:keys [tx app-route]}]
              {:pre [(not-any? nil? (keys tx))]}            ; tx :: {uri tx}    https://github.com/hyperfiddle/hyperfiddle/issues/816
              (let [with-actions (mapv (fn [[dbname tx]]
                                         (assert dbname)
                                         (let [tx (update-to-tempids get-state branch dbname tx)]
                                           [:with branch dbname tx]))
                                       tx)
                    parent-branch (branch/decode-parent-branch branch)]
                ; should the tx fn not be withd? if the transact! fails, do we want to run it again?
                (dispatch! (apply batch (concat with-actions on-start)))
                ;(with-groups rt invert-route parent-branch tx-groups :route app-route :post-tx nil)
                (let [tx-groups (->> (get-in (get-state) [::runtime/partitions branch :stage])
                                     (filter (fn [[dbname tx]] (and (should-transact!? dbname get-state) (not (empty? tx)))))
                                     (into {}))]
                  (if (and (nil? parent-branch) (not (empty? tx-groups)))
                    ; todo what if transact throws?
                    (transact! rt tx-groups dispatch! get-state
                               :post-tx (let [clear-uris (->> (keys tx-groups)
                                                              (map (fn [dbname] [:reset-stage-db branch dbname nil]))
                                                              vec)]
                                          (concat clear-uris ; clear the uris that were transacted
                                                  [[:merge branch] ; merge the untransacted uris up
                                                   (discard-partition branch)])) ; clean up the partition
                               :route app-route)
                    (either/branch
                      (or (some-> app-route route/validate-route+) (either/right nil)) ; arbitrary user input, need to validate
                      (fn [e]
                        (dispatch! (batch [:merge branch]
                                          [:set-error e]
                                          (discard-partition branch))))
                      (fn [app-route]
                        (let [actions [[:merge branch]
                                       (when app-route
                                         ; what about local-basis? why not specify branch?
                                         [:partition-route nil app-route])
                                       (discard-partition branch)]]
                          (dispatch! (apply batch (conj actions [:hydrate!-start parent-branch])))
                          (hydrate-partition rt parent-branch dispatch! get-state)))))))))))

(defn reset-stage-db [rt branch dbname tx]
  (fn [dispatch! get-state]
    ; check if auto-tx is OFF first?
    (when (not= tx (get-in (get-state) [::runtime/partitions branch :stage dbname]))
      (dispatch! (batch [:reset-stage-db branch dbname tx] [:hydrate!-start nil]))
      (hydrate-partition rt nil dispatch! get-state))))

(defn manual-transact-db! [rt dbname]
  (fn [dispatch! get-state]
    ; todo do something when child branches exist and are not nil: hyperfiddle/hyperfiddle#99
    ; can only transact one branch
    (let [tx-groups (-> (get-in (get-state) [::runtime/partitions nil :stage])
                        (select-keys [dbname]))]
      (transact! rt tx-groups dispatch! get-state :post-tx [[:reset-stage-db nil dbname]]))))
