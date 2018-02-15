(ns hyperfiddle.foundation.reducers
  (:require [hypercrud.client.tx :as tx]
            [hypercrud.util.branch :as branch]
            [hypercrud.util.core :as util]))


(defn hydrate-id-reducer [loading? action & args]
  (case action
    :hydrate!-start (first args)
    :hydrate!-success nil
    :popover-hydrate!-success nil
    :hydrate!-failure nil
    :popover-hydrate!-failure nil

    (or loading? false)))

(defn stage-reducer [stage action & args]
  (let [discard (fn [stage branch]
                  (dissoc stage branch))
        with (fn [stage branch uri tx]
               (update-in stage [branch uri] tx/into-tx tx))
        clean (fn [stage]
                (->> stage
                     (util/map-values (fn [multi-color-tx]
                                        (->> multi-color-tx
                                             (remove (fn [[uri tx]] (nil? tx)))
                                             (into {}))))
                     (remove (fn [[branch multi-color-tx]] (empty? multi-color-tx)))
                     (into {})))]
    (-> (case action
          :transact!-success nil

          :discard-branch (let [[branch] args]
                            (discard stage branch))

          :with (let [[branch uri tx] args]
                  (-> stage
                      (with branch uri tx)))

          :merge (let [[branch] args
                       parent-branch (branch/decode-parent-branch branch)]
                   (-> (reduce (fn [stage [uri tx]]
                                 (with stage parent-branch uri tx))
                               stage
                               (get stage branch))
                       (discard branch)))

          :reset-stage (first args)

          stage)
        clean)))

(defn route-reducer [route action & args]
  (case action
    :set-route (first args)

    ; todo we want to overwrite our current browser location with this new url
    ; currently this new route breaks the back button
    :transact!-success (first args)

    route))

(defn tempid-lookups-reducer [tempid-lookup action & args]
  (case action
    :hydrate!-success (second args)
    :popover-hydrate!-success (merge-with (partial merge-with merge) tempid-lookup (second args))
    :discard-branch (let [[branch] args]
                      (util/map-values #(dissoc % branch) tempid-lookup))
    tempid-lookup))

(defn ptm-reducer [ptm action & args]
  (case action
    :hydrate!-success (first args)
    :popover-hydrate!-success (merge ptm (first args))
    (or ptm nil)))

(defn error-reducer [error action & args]
  (case action
    :hydrate!-success nil
    ; need errors to be serializable, so crapily pr-str
    :set-error (pr-str (first args))
    :hydrate!-failure (pr-str (first args))
    error))

(defn popover-reducer [popovers action & args]
  (case action
    :open-popover (let [[popover-id] args]
                    (conj popovers popover-id))
    :close-popover (let [[popover-id] args]
                     (disj popovers popover-id))
    (or popovers #{})))

(defn branches-reducer [branches action & args]
  (case action
    :add-branch (let [[branch branch-aux route local-basis] args]
                  (assoc branches branch {:local-basis local-basis
                                          :route route
                                          :hyperfiddle.runtime/branch-aux branch-aux}))
    :discard-branch (let [[branch] args]
                      (dissoc branches branch))
    (or branches {})))

(defn pressed-keys-reducer [v action & args]
  (or v #{}))

(defn global-basis-reducer [global-basis action & args]
  (case action
    :set-global-basis (first args)
    global-basis))

(defn local-basis-reducer [local-basis action & args]
  (case action
    :set-local-basis (first args)
    local-basis))

(defn domain-reducer [domain action & args]
  (case action
    :hyperfiddle.runtime/set-domain (first args)
    domain))

(defn staging-open-reducer [staging-open? action & args]
  (case action
    :toggle-staging (not staging-open?)
    :hydrate!-failure true
    (or staging-open? false)))

(defn display-mode-reducer [display-mode action & args]
  (case action
    :toggle-display-mode (case display-mode
                           :xray :user
                           :user :xray)
    :set-display-mode (first args)
    (or display-mode :user)))

(def reducer-map {:display-mode display-mode-reducer
                  :staging-open staging-open-reducer
                  :hydrate-id hydrate-id-reducer
                  :route route-reducer
                  :global-basis global-basis-reducer
                  :local-basis local-basis-reducer
                  :hyperfiddle.runtime/domain domain-reducer
                  :stage stage-reducer
                  :ptm ptm-reducer
                  :error error-reducer
                  :branches branches-reducer
                  :popovers popover-reducer
                  :pressed-keys pressed-keys-reducer
                  :tempid-lookups tempid-lookups-reducer})
