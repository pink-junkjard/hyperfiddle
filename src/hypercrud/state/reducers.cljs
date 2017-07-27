(ns hypercrud.state.reducers
  (:require [hypercrud.browser.core :as browser]
            [hypercrud.client.tx :as tx]
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
  (let [discard (fn [stage conn-id branch]
                  (update stage conn-id dissoc branch))
        with (fn [stage conn-id branch tx]
               (update-in stage [conn-id branch] tx/into-tx tx))
        clean (fn [stage]
                (->> stage
                     (util/map-values (fn [branches]
                                        (->> branches
                                             (remove (fn [[branch tx]] (nil? tx)))
                                             (into {}))))
                     (remove (fn [[conn-id branches]] (empty? branches)))
                     (into {})))]
    (case action
      :transact!-success nil

      :discard (let [[conn-id branch] args]
                 (-> stage
                     (discard conn-id branch)
                     clean))

      :with (let [[conn-id branch tx] args]
              (-> stage
                  (with conn-id branch tx)
                  clean))

      :merge (let [[conn-id branch] args
                   parent-branch (branch/decode-parent-branch branch)
                   tx (get-in stage [conn-id branch])]
               (-> stage
                   (with conn-id parent-branch tx)
                   (discard conn-id branch)
                   clean))

      :reset-branch (let [[conn-id branch tx] args]
                      (-> stage
                          (assoc-in [conn-id branch] tx)
                          clean))

      :reset-stage (first args)

      stage)))

(defn route-reducer [route action & args]
  (case action
    :soft-set-route (first args)
    :hard-set-route (first args)

    ; todo we want to overwrite our current browser location with this new url
    ; currently this new route breaks the back button
    :transact!-success (browser/replace-tempids-in-route (first args) route)

    route))

(defn ptm-reducer [ptm action & args]
  (case action
    :set-ptm (first args)

    (or ptm nil)))

(defn error-reducer [error action & args]
  (case action
    :set-ptm nil
    :set-error (first args)
    :hydrate!-failure (first args)
    error))

(def hc-reducer-map {:hydrate-id hydrate-id-reducer
                     :route route-reducer
                     :stage stage-reducer
                     :ptm ptm-reducer
                     :error error-reducer})

(def hc-root-reducer (state/combine-reducers hc-reducer-map))
