(ns hypercrud.state.reducers
  (:require [hypercrud.client.tx :as tx]
            [hypercrud.state.core :as state]
            [hypercrud.util.branch :as branch]
            [hypercrud.util.core :as util]))

(defn hydrate-id-reducer [loading? action & args]
  (case action
    :hydrate!-start (first args)
    :hydrate!-success nil
    :hydrate!-failure nil

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
    (case action
      :transact!-success nil

      :discard (let [[branch] args]
                 (-> stage
                     (discard branch)
                     clean))

      :with (let [[branch uri tx] args]
              (-> stage
                  (with branch uri tx)
                  clean))

      :merge (let [[branch] args
                   parent-branch (branch/decode-parent-branch branch)]
               (-> (reduce (fn [stage [uri tx]]
                             (with stage parent-branch uri tx))
                           stage
                           (get stage branch))
                   (discard branch)
                   clean))

      :reset-stage (first args)

      stage)))

(defn route-reducer [route action & args]
  (case action
    :set-route (first args)

    ; todo we want to overwrite our current browser location with this new url
    ; currently this new route breaks the back button
    :transact!-success (first args)

    route))

(defn tempid-lookups-reducer [tempid-lookup action & args]
  (case action
    :set-ptm (second args)
    tempid-lookup))

(defn ptm-reducer [ptm action & args]
  (case action
    :set-ptm (merge ptm (first args))

    (or ptm nil)))

(defn error-reducer [error action & args]
  (case action
    :set-ptm nil
    :set-error (first args)
    :hydrate!-failure (first args)
    error))

(defn popover-reducer [popovers action & args]
  (case action
    :open-popover (let [[branch] args]
                    (conj popovers branch))
    :close-popover (let [[branch] args]
                     (disj popovers branch))
    (or popovers #{})))

(defn pressed-keys-reducer [v action & args]
  (or v #{}))

(def root-reducer-map
  {:hydrate-id hydrate-id-reducer
   :encoded-route route-reducer
   :stage stage-reducer
   :ptm ptm-reducer
   :error error-reducer
   :popovers popover-reducer
   :pressed-keys pressed-keys-reducer
   :tempid-lookups tempid-lookups-reducer})

(def root-reducer (state/combine-reducers root-reducer-map))
