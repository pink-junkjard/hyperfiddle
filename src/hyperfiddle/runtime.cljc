(ns hyperfiddle.runtime
  (:require
    [clojure.set :as set]
    [contrib.datomic]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.reducers :as reducers]
    [hyperfiddle.state :as state]))


(defprotocol HF-Runtime
  (domain [rt])
  (io [rt])
  (hydrate [rt branch request])

  ; actions
  (-set-route [rt branch route force-hydrate])
  )

(defprotocol State                                          ; internal
  (state [rt] [rt path]))

(defn dispatch! [rt action-or-func]                         ; internal
  (state/dispatch! (state rt) reducers/root-reducer action-or-func))

(defn attribute-renderer [rt branch ident] @(state rt [::partitions branch :attr-renderers ident]))

(defn set-route
  "Set the route of the given branch. This may or may not trigger IO.
  Returns a promise"
  ([rt branch route]
   (-set-route rt branch route false))
  ([rt branch route force-hydrate]
   (-set-route rt branch route force-hydrate)))

(defn get-route [rt branch]
  @(state rt [::partitions branch :route]))

(defn- get-tempid-lookup!
  ; todo this is too low level an api to be worth making public
  "Returns the tempid lookup for the given branch and dbname.
  May throw an exception (e.g. if the branch has an invalid stage)"
  [rt branch dbname]
  {:pre [(domain/valid-dbname? (domain rt) dbname)]}
  (some-> @(state rt [::partitions branch :tempid-lookups dbname])
          deref))

(defn id->tempid! [rt branch dbname id]
  ; todo what about if the tempid is on a higher branch in the uri?
  (if (contrib.datomic/tempid? id)
    id
    (let [id->tempid (get-tempid-lookup! rt branch dbname)]
      (get id->tempid id id))))

(defn tempid->id! [rt branch dbname tempid]
  ; todo what about if the tempid is on a higher branch in the uri?
  (if (contrib.datomic/tempid? tempid)
    (let [tempid->id (-> (get-tempid-lookup! rt branch dbname)
                         (set/map-invert))]
      (get tempid->id tempid tempid))
    tempid))

(defn create-branch [rt branch]
  (dispatch! rt [:create-partition branch]))

(defn discard-branch [rt branch]
  ; todo intermediary hack for circular deps while we move away from dispatch! as the public api
  (let [discard-branch (resolve 'hyperfiddle.actions/discard-partition)]
    (dispatch! rt (fn [dispatch! get-state] (discard-branch get-state branch)))))

(defn branch-is-loading? [rt branch]
  (some? @(state rt [::partitions branch :hydrate-id])))

(defn branch-exists? [rt branch]
  (some? @(state rt [::partitions branch])))

(defn set-branch-error [rt branch e]
  (dispatch! rt [:partition-error branch e]))

(defn with-tx
  "Stage tx to the given dbname.  This will rehydrate the given branch.  This may or may not immediately transact.
  Returns a promise"
  [rt branch dbname tx]
  ; todo intermediary hack for circular deps while we move away from dispatch! as the public api
  (let [with-groups (resolve 'hyperfiddle.actions/with-groups)]
    (with-groups rt branch {dbname tx})))

(defn commit-branch
  "Commit the branch to its parent.  This will rehydrate the parent branch.  This may or may not immediately transact.
  Returns a promise"
  [rt branch tx-groups on-start]
  ; todo intermediary hack for circular deps while we move away from dispatch! as the public api
  (let [commit-branch (resolve 'hyperfiddle.actions/commit-branch)]
    (commit-branch rt branch tx-groups on-start)))

(defn popover-is-open? [rt branch popover-id]
  (some? @(state rt [::partitions branch :popovers popover-id])))

(defn open-popover [rt branch popover-id]
  ; todo intermediary hack for circular deps while we move away from dispatch! as the public api
  (let [open-popover (resolve 'hyperfiddle.actions/open-popover)]
    (dispatch! rt (open-popover branch popover-id))))

(defn close-popover [rt branch popover-id]
  ; todo intermediary hack for circular deps while we move away from dispatch! as the public api
  (let [close-popover (resolve 'hyperfiddle.actions/close-popover)]
    (dispatch! rt (close-popover branch popover-id))))
