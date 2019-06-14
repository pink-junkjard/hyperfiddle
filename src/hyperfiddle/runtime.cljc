(ns hyperfiddle.runtime
  (:require
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
