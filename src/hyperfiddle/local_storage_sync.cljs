(ns hyperfiddle.local-storage-sync
  (:require
    [contrib.component :as component]
    [contrib.document :as document]
    [contrib.local-storage :as local-storage]
    [hyperfiddle.foundation :as foundation]
    [hyperfiddle.io.global-basis :as global-basis]
    [hyperfiddle.runtime :as runtime]
    [taoensso.timbre :as timbre]))


(def running-ls-schema-version 4)

(def ls-migrations
  {2 (fn [ls-state]                                         ; v3 removed non-nil stage from localstorage
       (assert (= 2 (:version ls-state)))
       (-> (select-keys ls-state [::runtime/auto-transact
                                  ::runtime/global-basis
                                  ::runtime/user-id
                                  :stage])
           (update :stage get nil)
           (assoc :version 3)))
   3 (fn [ls-state]                                         ; v4 add :last-modified
       (assert (= 3 (:version ls-state)))
       (assoc ls-state :version 4 :last-modified 0))        ; just use the epoch
   })

(defn- state->local-storage [state-val]
  (-> (select-keys state-val [::runtime/auto-transact
                              ::runtime/global-basis
                              ::runtime/user-id])
      (assoc :version running-ls-schema-version
             :stage (get-in state-val [::runtime/partitions nil :stage]))))

(defn- local-storage-state-watcher [k r o n]
  (let [o (state->local-storage o)
        n (state->local-storage n)]
    (when-not (= o n)
      ; only sync if the LS values have changed
      (let [ls-state (try (local-storage/get-item :STATE) (catch :default e nil))]
        ; implied trust that the localstorage listener has already synced other tabs with this one
        (when-not (= ls-state n)
          (local-storage/set-item! :STATE (assoc n :last-modified (.now js/Date))))))))

(defn- local-storage-event-action [rt e dispatch! get-state]
  (let [{:keys [::runtime/auto-transact ::runtime/global-basis ::runtime/user-id :stage :version]} (local-storage/get-item (.-storageArea e) :STATE)]
    (let [current-state (get-state)]
      (if (not= running-ls-schema-version version)
        (let [error "Hyperfiddle has been updated, please refresh!"]
          (when (not= (::runtime/fatal-error current-state) error)
            ; turn off localstorage sync, this tab is dead
            (remove-watch (runtime/state rt) :local-storage)
            (dispatch! [:set-error error])))
        (let [different-user (not= user-id (::runtime/user-id current-state))
              different-basis (not= global-basis (::runtime/global-basis current-state))
              different-stage (not= stage (get-in current-state [::runtime/partitions nil :stage]))
              different-autotx (not= auto-transact (::runtime/auto-transact current-state))
              init-level (cond
                           different-user foundation/LEVEL-NONE
                           different-basis foundation/LEVEL-GLOBAL-BASIS
                           different-stage foundation/LEVEL-LOCAL-BASIS
                           :else foundation/LEVEL-HYDRATE-PAGE)
              action (cond-> [:batch]
                       different-user (conj [:set-user-id user-id])
                       different-basis (conj [:set-global-basis global-basis])
                       different-stage (conj [:reset-stage-branch nil stage])
                       different-autotx (conj [:set-auto-transact auto-transact]))]
          (remove-watch (runtime/state rt) :local-storage)
          ; this dispatch! is syncronous, so we can safely, temporarily stop the localstorage sync
          ; this is desired so an inactive tab does NOT
          (dispatch! action)
          (add-watch (runtime/state rt) :local-storage local-storage-state-watcher)
          (foundation/bootstrap-data rt init-level foundation/LEVEL-HYDRATE-PAGE (document/root-rel-url!) (::runtime/global-basis current-state) different-stage))))))

(defn- init-auto-tx [ls ssr host-env]
  (reduce-kv (fn [acc k ssr-v]
               (let [ls-v (get ls k)]
                 (assoc acc k (cond
                                (false? ssr-v) false        ; ssr false trumps local storage
                                (some? ls-v) ls-v           ; user may have opted in to auto-tx
                                (:active-ide? host-env) false ; ignore ssr and default to false
                                :else ssr-v                 ; no ide (likely alias so no visible staging area), trust ssr
                                ))))
             {}
             ssr))

(defrecord LocalStorageSync [rt event-listener]
  component/Lifecycle
  (start [component]
    (let [initial-state @(runtime/state rt)
          ls-state (try (local-storage/get-item :STATE) (catch :default e {}))
          new-ls-state (loop [ls-state ls-state]
                         (if (= running-ls-schema-version (:version ls-state))
                           (-> (select-keys ls-state [::runtime/auto-transact
                                                      ::runtime/global-basis
                                                      ::runtime/user-id
                                                      :last-modified
                                                      :stage
                                                      :version])
                               (update ::runtime/auto-transact init-auto-tx (::runtime/auto-transact initial-state) (runtime/host-env rt))
                               (assoc ::runtime/user-id (::runtime/user-id initial-state)) ; ssr always win (it has access to cookies)
                               (update ::runtime/global-basis (fn [gb]
                                                                (if (<= 0 (global-basis/compare (::runtime/global-basis initial-state) gb))
                                                                  (::runtime/global-basis initial-state)
                                                                  gb))))
                           (if-let [migrate (get ls-migrations (:version ls-state))]
                             (recur (migrate ls-state))
                             (do
                               (when-not (nil? ls-state) (timbre/error "Unable to migrate local-storage: " ls-state))
                               (-> (state->local-storage initial-state)
                                   (assoc :last-modified (.now js/Date))
                                   (update ::runtime/auto-transact #(init-auto-tx nil % (runtime/host-env rt))))))))]
      (when-not (= ls-state new-ls-state)
        (local-storage/set-item! :STATE new-ls-state))
      ; todo, we should be dispatching
      (reset! (runtime/state rt)
              (-> (merge initial-state (dissoc new-ls-state :stage :version :last-modified))
                  (assoc-in [::runtime/partitions nil :stage] (:stage new-ls-state)))))

    (let [event-listener (fn [e] (runtime/dispatch! rt (partial local-storage-event-action rt e)))]
      (.addEventListener js/window "storage" event-listener)
      (add-watch (runtime/state rt) :local-storage local-storage-state-watcher)
      (assoc component :event-listener event-listener)))

  (stop [component]
    (.removeEventListener js/window "storage" event-listener)
    (remove-watch (runtime/state rt) :local-storage)
    component))
