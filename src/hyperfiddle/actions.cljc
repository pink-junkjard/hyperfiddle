(ns hyperfiddle.actions
  (:require
    [cats.core :refer [mlet]]
    [cats.labs.promise]
    [clojure.spec.alpha :as s]
    [contrib.data :as data]
    [contrib.datomic-tx :as tx]
    [hyperfiddle.branch :as branch]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.io.core :as io]
    [hyperfiddle.route :as route]
    [hyperfiddle.runtime :as runtime]
    [promesa.core :as p]
    [taoensso.timbre :as timbre]))


; batch doesn't make sense with thunks (can be sync or async dispatches in a thunk),
; user beware
(defn batch [& action-list] (cons :batch action-list))

(defn hydrate-partition [io branch dispatch! get-state]
  (let [partitions (::runtime/partitions (get-state))
        {:keys [hydrate-id local-basis route]} (get partitions branch)
        stage (data/map-values :stage partitions)]
    ; todo only grab applicable stages (my own and all my parents')
    (-> (io/hydrate-route io local-basis route branch stage)
        (p/then (fn [{:keys [local-basis attr-renderers project ptm schemas tempid-lookups]}]
                  (if (= hydrate-id (get-in (get-state) [::runtime/partitions branch :hydrate-id]))
                    (dispatch! [:hydrate!-route-success branch attr-renderers project ptm schemas tempid-lookups local-basis])
                    (timbre/info (str "Ignoring response for " hydrate-id)))))
        (p/catch (fn [error]
                   (if (= hydrate-id (get-in (get-state) [::runtime/partitions branch :hydrate-id]))
                     (dispatch! [:partition-error branch error])
                     (timbre/info (str "Ignoring response for " hydrate-id)))
                   (throw error))))))

(defn refresh-global-basis [io on-finally dispatch! get-state] ; on-finally is a crude hack
  (-> (io/global-basis io)
      (p/then (fn [global-basis]
                (dispatch! (apply batch [:set-global-basis global-basis] on-finally))))
      (p/catch (fn [error]
                 (dispatch! (apply batch [:set-error error] on-finally))
                 (throw error)))))

(defn refresh-partition-basis [io branch dispatch! get-state]
  (let [{:keys [::runtime/global-basis ::runtime/partitions]} (get-state)
        route (get-in partitions [branch :route])]
    (-> (io/local-basis io global-basis route)
        (p/then (fn [local-basis]
                  (dispatch! [:partition-basis branch local-basis])))
        (p/catch (fn [error]
                   (dispatch! [:partition-error branch error])
                   (throw error))))))

(def LEVEL-NONE 0)
(def LEVEL-GLOBAL-BASIS 1)
(def LEVEL-LOCAL-BASIS 2)
(def LEVEL-HYDRATE-PAGE 3)

(defn bootstrap-data [rt branch init-level & {:keys [hydrate-page?] :or {hydrate-page? (constantly true)}}]
  (let [dispatch! (partial runtime/dispatch! rt)
        get-state #(deref (runtime/state rt))
        io (runtime/io rt)]
    (cond-> (p/resolved nil)
      (< init-level LEVEL-GLOBAL-BASIS) (p/then (fn [_] (refresh-global-basis io nil dispatch! get-state)))
      (< init-level LEVEL-LOCAL-BASIS) (p/then (fn [_] (refresh-partition-basis io branch dispatch! get-state)))
      (< init-level LEVEL-HYDRATE-PAGE) (p/then (fn [_]
                                                  (dispatch! [:hydrate!-start branch])
                                                  (if (hydrate-page?)
                                                    (hydrate-partition io branch dispatch! get-state)
                                                    (p/resolved (dispatch! [:hydrate!-shorted branch]))))))))

(declare discard-partition)

(defn discard-child-partitions [get-state branch-id]
  (->> (get-in (get-state) [::runtime/partitions branch-id ::branch/children])
       (mapcat (partial discard-partition get-state))))

(defn discard-partition [get-state branch-id]
  (cons [:discard-partition branch-id]
        (discard-child-partitions get-state branch-id)))

(defn close-popover [branch popover-id]
  [:close-popover branch popover-id])

(defn set-route
  ([rt branch route force-hydrate]
   (p/promise
     (fn [resolve reject]
       (runtime/dispatch! rt (fn [dispatch! get-state]
                               (p/branch
                                 (set-route rt branch route force-hydrate dispatch! get-state)
                                 resolve reject))))))
  ([rt branch route force-hydrate dispatch! get-state]
   {:pre [(s/valid? :hyperfiddle/route route)]}
   (let [current-route (get-in (get-state) [::runtime/partitions branch :route])]
     (if (and (not force-hydrate) (route/equal-without-frag? route current-route))
       (do
         (dispatch! [:partition-route branch route])        ; just update state without re-hydrating
         (p/resolved nil))
       (do
         (dispatch! (apply batch
                           [:partition-route branch route]
                           [:close-all-popovers branch]
                           (discard-child-partitions get-state branch)))
         (bootstrap-data rt branch LEVEL-GLOBAL-BASIS))))))

(defn- update-to-tempids! [get-state branch dbname tx]
  (let [{:keys [tempid-lookups schemas]} (get-in (get-state) [::runtime/partitions branch])
        schema @(get schemas dbname)
        id->tempid (some-> (get tempid-lookups dbname) deref)]
    (map (partial tx/stmt-id->tempid id->tempid schema) tx)))

(defn transact! [rt branch-id tx-groups dispatch! get-state & {:keys [post-tx]}]
  {:pre [(domain/valid-dbnames? (runtime/domain rt) (keys tx-groups))]}
  (dispatch! [:transact!-start])
  (-> (io/transact! (runtime/io rt) tx-groups)
      (p/catch (fn [e]
                 (dispatch! [:transact!-failure e])
                 (throw e)))
      (p/then (fn [{:keys [tempid->id]}]
                ; todo should just call foundation/bootstrap-data
                (mlet [:let [on-finally (into [[:transact!-success branch-id (keys tx-groups)]] post-tx)]
                       _ (refresh-global-basis (runtime/io rt) on-finally dispatch! get-state)
                       :let [invert-id (fn [dbname id] (get-in tempid->id [dbname id] id))
                             route (-> (get-in (get-state) [::runtime/partitions branch-id :route])
                                       (route/invert-route invert-id))]]
                  ; todo we want to overwrite our current browser location with this new url
                  ; currently this new route breaks the back button
                  (runtime/set-route rt branch-id route true))))))

(defn should-transact!? [dbname get-state]
  (get-in (get-state) [::runtime/auto-transact dbname]))

(defn with-groups
  ([rt branch tx-groups]
   (p/promise
     (fn [resolve reject]
       (runtime/dispatch! rt (fn [dispatch! get-state]
                               (p/branch
                                 (with-groups rt branch tx-groups dispatch! get-state)
                                 resolve reject))))))
  ([rt branch tx-groups dispatch! get-state]
   {:pre [(domain/valid-dbnames? (runtime/domain rt) (keys tx-groups))]}
   (let [tx-groups (->> tx-groups
                        (remove (fn [[dbname tx]] (empty? tx)))
                        (map (fn [[dbname tx]] [dbname (update-to-tempids! get-state branch dbname tx)]))
                        (into {}))
         transact-dbnames (->> (keys tx-groups)
                               (filter (fn [dbname] (and (branch/root-branch? branch) (should-transact!? dbname get-state)))))
         transact-groups (select-keys tx-groups transact-dbnames)
         with-actions (->> (apply dissoc tx-groups transact-dbnames)
                           (mapv (fn [[dbname tx]] [:with branch dbname tx])))]
     (if (not (empty? transact-groups))
       ; todo what if transact throws?
       (transact! rt branch transact-groups dispatch! get-state
                  :post-tx with-actions)
       (do
         (dispatch! (apply batch (conj with-actions [:hydrate!-start branch])))
         (hydrate-partition (runtime/io rt) branch dispatch! get-state))))))

(defn open-popover [branch popover-id]
  [:open-popover branch popover-id])

(defn commit-branch
  ([rt branch tx-groups on-start]
   (p/promise
     (fn [resolve reject]
       (runtime/dispatch! rt (fn [dispatch! get-state]
                               (p/branch
                                 (commit-branch rt branch tx-groups on-start dispatch! get-state)
                                 resolve reject))))))
  ([rt branch tx-groups on-start dispatch! get-state]       ; todo rewrite in terms of with-groups
   {:pre [(domain/valid-dbnames? (runtime/domain rt) (keys tx-groups))]}
   (let [with-actions (->> tx-groups
                           (remove (fn [[dbname tx]] (empty? tx)))
                           (mapv (fn [[dbname tx]]
                                   (let [tx (update-to-tempids! get-state branch dbname tx)]
                                     [:with branch dbname tx]))))
         parent-branch (branch/parent-branch-id branch)]
     ; should the tx fn not be withd? if the transact! fails, do we want to run it again?
     (dispatch! (apply batch (concat with-actions on-start)))
     ;(with-groups rt parent-branch tx-groups :route route)
     (let [tx-groups (->> (get-in (get-state) [::runtime/partitions branch :stage])
                          (filter (fn [[dbname tx]] (and (should-transact!? dbname get-state) (not (empty? tx)))))
                          (into {}))]
       (if (and (branch/root-branch? parent-branch) (not (empty? tx-groups)))
         ; todo what if transact throws?
         (transact! rt parent-branch tx-groups dispatch! get-state
                    :post-tx (let [clear-uris (->> (keys tx-groups)
                                                   (map (fn [dbname] [:reset-stage-db branch dbname nil]))
                                                   vec)]
                               (concat clear-uris           ; clear the uris that were transacted
                                       [[:merge branch]     ; merge the untransacted uris up
                                        (discard-partition get-state branch)]))) ; clean up the partition
         (do
           (dispatch! (batch [:merge branch]
                             (discard-partition get-state branch)
                             [:hydrate!-start parent-branch]))
           (hydrate-partition (runtime/io rt) parent-branch dispatch! get-state)))))))

(defn reset-stage-db [rt branch dbname tx]
  {:pre [(domain/valid-dbname? (runtime/domain rt) dbname)]}
  (fn [dispatch! get-state]
    ; check if auto-tx is OFF first?
    (when (not= tx (get-in (get-state) [::runtime/partitions branch :stage dbname]))
      (dispatch! (batch [:reset-stage-db branch dbname tx] [:hydrate!-start branch]))
      (hydrate-partition (runtime/io rt) branch dispatch! get-state))))

(defn manual-transact-db! [rt branch-id dbname]
  {:pre [(domain/valid-dbname? (runtime/domain rt) dbname)]}
  (fn [dispatch! get-state]
    ; todo do something when child branches exist and are not nil: hyperfiddle/hyperfiddle#99
    ; can only transact one branch
    (let [tx-groups (-> (get-in (get-state) [::runtime/partitions branch-id :stage])
                        (select-keys [dbname]))]
      (transact! rt branch-id tx-groups dispatch! get-state :post-tx [[:reset-stage-db branch-id dbname]]))))
